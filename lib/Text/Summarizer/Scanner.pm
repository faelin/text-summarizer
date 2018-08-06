package Text::Summarizer::Scanner;

use v5.14;
use strict;
use warnings;
use Moo;
use Types::Standard qw/ Bool Ref Str Int Num InstanceOf Bool FileHandle /;
use List::AllUtils qw/ max min sum sum0 singleton pairkeys pairvalues pairs all /;
use Algorithm::CurveFit;
use Lingua::Stem qw/ stem /;
use Text::Typifier;
use utf8;

binmode STDOUT, ':utf8';

use vars qw(@ISA @EXPORT @EXPORT_OK %EXPORT_TAGS $VERSION);
require Exporter;

@ISA = qw(Exporter);
@EXPORT = qw();
@EXPORT_OK = qw();
%EXPORT_TAGS = (all => [@EXPORT_OK]);
$VERSION = '2.10';



has articles_path => (
    is => 'rw',
    isa => Str,
    default => 'articles/*'
);

has permanent_path => (
    is  => 'rw',
    isa => Str,
    default => 'data/permanent.stop',
);

has stopwords_path => (
    is  => 'rw',
    isa => Str,
    default => 'data/stopwords.stop',
);

has typifier => (
    is => 'rw',
    isa => InstanceOf["Text::Typifier"],
    default => sub { Text::Typifier->new(); },
);

has store_scanner => (
	is => 'rw',
	isa => Bool,
	default => 0,
);

has print_dest => (
    is => 'rw',
    isa => FileHandle,
    lazy => 1,
    default => sub { open(my $fh, '>-') or die "Can't open >- STDOUT: $!"; $fh },
);

has return_count => (
    is => 'rw',
    isa => Num,
    default => 20,
);

has phrase_thresh => (
    is => 'rw',
    isa => Int,
    default => 2,
);

has phrase_radius => (
    is => 'rw',
    isa => Int,
    default => 5,
);

has freq_constant => (
    is => 'rw',
    isa => Num,
    default => 0.004,
);

has watch_count => (
    is => 'rwp',
    isa => Int,
    default => 0,
);

has article_length => (
    is => 'rwp',
    isa => Int,
    default => 0,
    lazy => 1,
);

has [qw/ print_scanner print_graphs print_working /] => (
    is => 'rw',
    isa => Bool,
    default => 0,
);

has [qw/ full_text file_name text_hint /] => (
    is => 'rwp',
    isa => Str,
);

has [qw/ types_list paragraphs sentences sen_words word_list frag_list /] => (
    is => 'rwp',
    isa => Ref['ARRAY'],
);

has [qw/ type_scores freq_hash clst_hash phrs_hash sigma_hash inter_hash score_hash phrs_list stopwords /] => (
    is => 'rwp',
    isa => Ref['HASH'],
);

has watchlist => (
    is => 'rwp',
    isa => Ref['HASH'],
    clearer => 1,
);



sub scan_text {
    my ($self, $text, $path) = @_;

    $self->_set_file_name( '' );
    $self->_set_text_hint( '' );

    if ( ref $text ) {
        $self->_set_file_name( $path );
        $text = join "\n" => map { $_ } <$text>;
    }

    $self->_set_text_hint( '"' . substr($text,0,50) . '...' . substr($text,-30) . '"' );

    $self->_build_stopwords;

    $self->tokenize( $text ); #breaks the provided file into sentences and individual words

    $self->_build_freq_hash;
    $self->_build_clst_hash;
    $self->_build_phrs_hash;
    $self->_build_sigma_hash;
    $self->_build_frag_list;
 
    $self->develop_stopwords; #analyzes the frequency and clustering of words within the provided file

	$self->write_scanner if ($self->store_scanner);

    return $self;
}

sub scan_file {
    my ($self, $file) = @_;
    my $fh;

    if ( ref $file ) {
        $fh = $file;

        return $self->scan_text( $fh, '<unknown path>' );

    } else {
        open( $fh, '<:utf8', $file )
            or die "Can't open file $file for summarizing: $!";

        return $self->scan_text( $fh, $file );
    }
}

sub scan_each {
    my ($self, $dir_path) = @_;

	$self->_build_stopwords;

	my @stopwords_ref;

  	for ( glob($dir_path // $self->articles_path) ) {
  		my $filepath = $_;

  		open( my $fh, '<:utf8', $filepath )
            or die "Can't open file $filepath for summarizing: $!";

	    $self->_set_file_name( '' );
	    $self->_set_text_hint( '' );

	    $self->_set_file_name( $filepath );
	    my $text = join "\n" => map { $_ } <$fh>;

	    $self->_set_text_hint( '"' . substr($text,0,50) . '...' . substr($text,-30) . '"' );

	    $self->tokenize( $text ); #breaks the provided file into sentences and individual words

	    $self->_build_freq_hash;
	    $self->_build_clst_hash;
	    $self->_build_phrs_hash;
	    $self->_build_sigma_hash;
	    $self->_build_frag_list;
	 
	    $self->develop_stopwords; #analyzes the frequency and clustering of words within the provided file

	    $self->write_scanner if ($self->store_scanner);

	    push @stopwords_ref => $self->watchlist;
  	}

    return @stopwords_ref;
}



sub tokenize {
    my ( $self, $text ) = @_;

    my @types_list = $self->typifier->typify($text);
    my @paragraphs = pairkeys @types_list;
    my %type_scores;
 
    foreach my $category ( pairvalues @types_list ) {
       foreach ( pairs @$category ) {
            my ( $type, $scraps ) = @$_;
            $type_scores{lc $_} += ($self->typifier->type_factors->{$type} // 1) for
               grep { !$self->stopwords->{lc $_} and /[A-Za-z]/ } map { /\b[\w-]+\b/gx } (ref $scraps eq 'ARRAY' ? @$scraps : $$scraps );
        }
    }

    $text = join "\n" => pairkeys @types_list;
    my $sentence_match = qr/(?|   (?<=(?<!\s[A-Z][a-z]) (?<!\s[A-Z][a-z]{2}) \. (?![A-Z0-9]\.|\s[a-z0-9]) | \! | \?) (?:(?=[A-Z])|\s+)
                              |   (?: \n+ | ^\s+ | \s+$ )
                            )/mx;

    my @sentences = split /$sentence_match/ => $text; # array of sentences
    my @word_list; # array literal of all the words in the entire text body
    my @sen_words; # array reference to all of the tokens in each sentence
    for (@sentences) {  #creates an array of each word in the current article
        my @words = grep { /[A-Za-z]/ } map { /\b (?: \w \. (?: ['’-] \w+ )?)+ | (?: \w+ ['’-]? )+ (?=\s|\b)/gx } lc $_;
            #tokenizes each sentence into complete words (single-quotes are considered part of the word they attach to)
        push @word_list =>  @words;
        push @sen_words => \@words;
    }

    $self->_set_article_length( scalar @word_list ); #counts the total number of words in the article
    $self->_set_full_text( $text  );
    $self->_set_sentences( \@sentences );
    $self->_set_word_list( \@word_list );
    $self->_set_sen_words( \@sen_words );
    $self->_set_paragraphs( \@paragraphs );
    $self->_set_types_list( \@types_list );
    $self->_set_type_scores( \%type_scores);

    return $self;
}



sub _build_stopwords {
    my $self = shift;
    my %stopwords;

    open( my $permanent_file, '<', $self->permanent_path )
        or die "Can't open stopwords permanent file " . $self->permanent_path . ": $!";
    chomp and $stopwords{ $_ } = 1 for (<$permanent_file>);
    close $permanent_file;
    
    open( my $stopwords_file, '<', $self->stopwords_path )
        or die "Can't open stopwords scanner file" . $self->stopwords_path . ": $!";
    chomp and $stopwords{ $_ } = 1 for (<$stopwords_file>);
    close $stopwords_file;

    return $self->_set_stopwords( \%stopwords );
}


sub write_scanner {
    my $self = shift;
    open( my $stopwords_file, ">:utf8", $self->stopwords_path)
        or die "Can't open stopwords scanner file " . $self->stopwords_path . ": $!";
    grep { print $stopwords_file "$_\n" } sort keys %{$self->watchlist};
    close $stopwords_file;

    return $self;
}



sub _build_freq_hash {
    my $self = shift;

    my $min_freq_thresh = int($self->article_length * $self->freq_constant) // 1; #estimates a minimum threshold of occurence for frequently occuring words
    my %freq_hash; #counts the number of times each word appears in the *%word_list* hash

    for my $word (@{$self->word_list}) {
        $freq_hash{$word}++ unless $self->stopwords->{$word} or $word !~ /[A-Za-z]/;
    }

    grep { delete $freq_hash{$_} if $freq_hash{$_} < $min_freq_thresh } keys %freq_hash; #remove words that appear less than the *$min_freq_thresh*
    
    $self->_set_freq_hash( \%freq_hash );

    return $self;
}



sub _build_clst_hash {
    my $self = shift;
    my (%cluster_hash, %cluster_count);

    my $abs_pos = 0;
    for my $sen_index (0..$#{$self->sentences}) { #gives the index of each sentence in the article
        my @sen_words = @{$self->sen_words->[$sen_index]}; # creates an array of each word in the given sentence

        for my $position (0..$#sen_words) { #iterates across each word in the sentence
            $abs_pos++;

            if ( exists $self->freq_hash->{$sen_words[$position]}) { # true if the given word at index *position* appears in the *freq_hash*
                my %word = ( abs => $abs_pos, sen => $sen_index, rel => $position, cnt => $cluster_count{$sen_words[$position]}++ );
                    # hash-vector of the following elements:
                    #  abs => absolute position of the currrent word within the entire token-stream
                    #  sen => the index of the current sentence
                    #  rel => position of the current word within the current sentence
                    #  cnt => number of times the given word has appeared in the entire text file 
                push @{$cluster_hash{$sen_words[$position]}} => \%word;
            }
        }
    }

    $self->_set_clst_hash( \%cluster_hash );

    return $self;
}



sub _build_phrs_hash {
    my $self = shift;

    #create long-form phrases around frequently used words by tracking forward and backward *phrase_radius* from any given *c_word*
    my %phrase_hash;
    for my $c_word (keys %{$self->clst_hash}) {
        for my $c_vector (@{$self->clst_hash->{$c_word}}) {
            my ($sen, $pos, $cnt) = @$c_vector{'sen', 'rel', 'cnt'};
                # *sen* indicates which sentence the current *c_word* appears in
                # *pos* indicates the position of the *c_word* within the sentence (see above)
                # *cnt* counts the total number of times the word has been detected thus far

            my @phrase = [ @{$self->sen_words->[$sen]}[ max($pos - $self->phrase_radius, 0) .. min($pos + $self->phrase_radius, scalar(@{$self->sen_words->[$sen]}) - 1) ] ];
               #array slice containing only tokens within *phrase_radius* of the *c_word* within the given sentence

            unshift @phrase => \$self->sentences->[$sen]; #begins the *phrase* array with a complete, unedited sentence (for reference only)
            push @{$phrase_hash{$c_word}} => \@phrase if scalar @{$phrase[-1]} > $self->phrase_thresh + 1;
               #the *phrase_hash* can only contain a given *phrase* array if it is longer than the defined *phrase_thresh* + 1  (defaults to 3)
        }
    }
    
    $self->_set_phrs_hash( \%phrase_hash );
    
    return $self;
}



sub _build_sigma_hash {
    my $self = shift;

    #determine population standard deviation for word clustering
    my %sigma_hash;
    for my $c_word (keys %{$self->clst_hash}) {
        for my $c_vector (@{$self->clst_hash->{$c_word}}) {

            #create a list of the distances between each instance of the current *c_word*
            my %dist_list;
            my ($L_pos, $R_pos);
            for (my $i = 0; $i < scalar @{$self->clst_hash->{$c_word}}; $i++) {
                $R_pos = $self->clst_hash->{$c_word}->[$i]->{abs};
                my $dist = $R_pos - ($L_pos // $R_pos);
                push @{$dist_list{$c_word}} => $dist if $dist >= 0;
                $L_pos = $R_pos;
            }

            #the following is used for scoring purposes, and is used only to determine the *sigma* score (population standard deviation) of the given *c_word*
            my $pop_size = scalar @{$dist_list{$c_word}} or 1;
            my $pop_ave  = sum0( @{$dist_list{$c_word}} ) / $pop_size;
            $sigma_hash{$c_word} = int sqrt( sum( map { ($_ - $pop_ave)**2 } @{$dist_list{$c_word}} ) / $pop_size ); #pop. std. deviation
        }
    }

    $self->_set_sigma_hash( \%sigma_hash );

    return $self;
}



sub _build_frag_list {
    my $self = shift;
    my @frag_list;

    F_WORD: for my $f_word (keys %{$self->phrs_hash}) {   #find common phrase-fragments
        my %full_phrase; #*inter_hash* contains phrase fragments;
        my (@hash_list, %sums_hash, %words_count);
        #  *hash_list* contains ordered, formatted lists of each word in the phrase fragment;
        #  *sums_hash* contains the total number of times each word appears in all phrases for the given *f_word*

        ORDER: for my $phrase (@{$self->phrs_hash->{$f_word}}) {
            my $sentence_ref  = $phrase->[0];
            my %ordered_words = map { $sums_hash{$phrase->[-1]->[$_]}++; ($_ => $phrase->[-1]->[$_]) } (0..$#{$phrase->[-1]});
               # *words* contains an ordered, formatted list of each word in the given phrase fragment, looks like:
               #  '01' => 'some'
               #  '02' => 'word'
               #  '03' => 'goes'
               #  '04' => 'here'

            $words_count{$_}++ for values %ordered_words;
            push @hash_list => { f_word => $f_word, sentence => $sentence_ref, counts => \%words_count, ordered => \%ordered_words };
        }

        #removes each word from the *word_hash* unless it occurs more than once amongst all phrases
        SCRAP: for my $word_hash (@hash_list) {
            for my $index ( keys %{$word_hash->{'ordered'}} ) {
                delete $word_hash->{'ordered'}->{$index} unless $sums_hash{$word_hash->{'ordered'}->{$index}} > 1
            }
        }

        #break phrases fragments into "scraps" (consecutive runs of words within the fragment)
        FRAG: for my $word_hash (@hash_list) {
            my (%L_scrap, %R_scrap); #a "scrap" is a sub-fragment
            my ($prev, $curr, $next) = (-1,0,0); #used to find consecutive sequences of words

            my $real = 0; #flag for stopwords identification

            my @word_keys = sort { $a <=> $b } keys %{$word_hash->{'ordered'}}; # *word_keys* contains a series of index-values
            for (my $i = 0; $i < scalar @word_keys; $i++ ) {
                $curr = $word_keys[$i];
                $next = $word_keys[$i+1] if $i < $#word_keys; # if-statement prevents out-of-bounds error

                if ( $next == $curr + 1 or $curr == $prev + 1 ) {
                    unless ($curr == $prev + 1) {  #resets *R_scrap* when the *curr* index skips over a number (i.e. a new scrap is encountered)
                        %L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
                        %R_scrap = (); #resets the *R_scrap*
                    }

                    $R_scrap{$curr} = $word_hash->{'ordered'}->{$curr};
                    $real = 1 unless $self->stopwords->{$R_scrap{$curr}}; #ensures that scraps consisting only of stopwords are ignored
                } else {
                    %L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
                    %R_scrap = (); #resets the *R_scrap*
                }

                $prev = $curr;
            }

            %L_scrap = %R_scrap if keys %L_scrap <= keys %R_scrap; #chooses the longest or most recent scrap
            %R_scrap = (); #resets the *R_scrap*

            push @frag_list => { %{$word_hash}, scrap => \%L_scrap } if $real and scalar keys %L_scrap >= $self->phrase_thresh;
        }
    }

    $self->_set_frag_list( \@frag_list );
    return $self;
}



sub develop_stopwords {
    my $self = shift;

    my %score_hash; #*score_hash* contains score values for words in those phrases
    $score_hash{$_}++ for keys %{$self->phrs_hash};
    JOIN: for my $fragment (@{$self->frag_list}) {

        #compile scraps for scoring
        my $scrap  = join ' ' => map { $score_hash{$fragment->{'scrap'}->{$_}}++;
        $fragment->{'scrap'}->{$_} } sort { $a <=> $b } keys %{$fragment->{'scrap'}};

        for my $word (split ' ' => $scrap) {
            $score_hash{$word} += $self->freq_hash->{$word} // 0;
            $score_hash{$word} += $self->sigma_hash->{$word} // 0;
            $score_hash{$word} -= $fragment->{'counts'}->{$word} // 0;
        }
    } 
    grep { delete $score_hash{$_} if $self->stopwords->{$_} } keys %score_hash;


    my @word_keys = sort { $score_hash{$b} <=> $score_hash{$a} or $a cmp $b } keys %score_hash;
    my $highest = $score_hash{$word_keys[0]};
    my $longest = max map { length } @word_keys;

    $score_hash{$_} = 40 * $score_hash{$_} / $highest for keys %score_hash;
    @word_keys = reverse grep { $score_hash{$_} >= 1 } @word_keys;

    my @scores = map { $score_hash{$_} } @word_keys;
    my @low    = @scores[ 0..(int scalar @scores / 2 - 1.5) ];
    my @high   = @scores[ (int scalar @scores / 2 + 1)..(int scalar @scores - 1) ];
    my @LM     = @low[  (int scalar @low / 2 - 0.5)..(int scalar @low / 2)   ];
    my @UM     = @high[ (int scalar @high / 2 - 0.5)..(int scalar @high / 2) ];
    my $Q1     = sum( @LM ) / scalar @LM;
    my $Q3     = sum( @UM ) / scalar @UM;
    my $IQR    = $Q3 - $Q1;
    my $lower  = $Q1;
    my $upper  = $Q3 + 1.5 * $IQR;

    my @graph_data = grep { $_ > $lower and $_ < $upper } map { $score_hash{$_} } @word_keys;
    my $n = scalar @graph_data;

    if ($n > 4) {
        my $average = sum( @graph_data ) / $n;
        my @xdata = 1..$n; # The data corresponsing to $variable
        my @ydata = @graph_data; # The data on the other axis
        my $max_iter = 100; # maximum iterations
        my @params_line = (
            # Name      Guess      Accuracy
            ['a',       0,         0.00001],
            ['b',       $average,  0.00001],
            ['c',       $highest,  0.00001],
        );

        Algorithm::CurveFit->curve_fit(
            formula             =>  'a + b * x + c * x^2',
            params              =>  \@params_line,
            xdata               =>  \@xdata,
            ydata               =>  \@ydata,
            maximum_iterations  =>  $max_iter,
        );

        my ($a, $b, $c) = ($params_line[0]->[1],$params_line[1]->[1],$params_line[2]->[1]);
        my %watchlist = %{$self->watchlist // {} };

        KEY: for my $index ( reverse 1..scalar @word_keys ) {
            my $score  = $a + $b * $index + $c * $index**2;
            $watchlist{$word_keys[$index - 1]}++ if $score >= $lower and $score < $score_hash{$word_keys[$index - 1]};
        }

        $self->_set_watchlist( \%watchlist );


        if ($self->print_scanner) {
            say "\nSCANNING:\n" . $self->full_text if $self->print_working;

            say "\n\n———————————————————————————————————————————\n\n";

            say "[file name] " . $self->file_name if $self->file_name;
            say "[text hint] " . $self->text_hint;

            say "\n---SCANNER GRAPHS---\n";

            say "KNOWN:";
            KEY: for my $index ( reverse 0..$#word_keys ) {
                my $format = "%" . $longest . "s|%s\n";
                my $score = $score_hash{$word_keys[$index]};
                my $score_string = sprintf " %5.2f |" => $score;
                for (0..max($score, $upper)) {
                    if ($score > $lower and $score < $upper) {
                        $score_string .= '+' if $_ <= $score;
                    } else {
                        $score_string .= ']' if $_ == int $upper;
                        $score_string .= '-' if $_ <= int $score;
                        $score_string .= ' ' if $_ >  int $score;
                        $score_string .= '[' if $_ == int $lower;
                    }
                }
                printf $format => ($word_keys[$index], $score_string);
            }

            printf "\n[whiskers] lower = %.2f; upper = %.2f\n\n" => ($lower, $upper);

            say "CALCULATED:";
            KEY: for my $index ( reverse 1..scalar @word_keys ) {
                my $format = "%" . $longest . "s|%s\n";
                my $score  = $a + $b * $index + $c * $index**2;
                my $score_string = sprintf " %5.2f |%s" => $score,
                    ($score >= $lower and $score < $score_hash{$word_keys[$index - 1]} ? '-' x $score : '+' x $score);
                printf $format => $word_keys[$index - 1], $score_string;
            }
            say "\n";
        }
    }

    return $self;
}



1;

__END__