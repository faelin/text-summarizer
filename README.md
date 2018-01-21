# NAME

Text::Summarizer - Summarize Bodies of Text

# SYNOPSIS

	use Text::Summarizer;
	
	my $summarizer = Text::Summarizer->new;
	
	my $summary   = $summarizer->summarize_file("articles/article00.txt");
		#or if you want to process in bulk
	my @summaries = $summarizer->summarize_all("articles/*");
	
	$summarizer->pretty_print($summary);
	$summarizer->pretty_print($_) for (@summaries);

# DESCRIPTION

This module allows you to summarize bodies of text into a scored hash of  _sentences_,  _phrase-fragments_, and  _individual words_ from the provided text. These scores reflect the weight (or precedence) of the relative text-fragments, i.e. how well they summarize or reflect the overall nature of the text. All of the sentences and phrase-fragments are drawn from within the existing text, and are NOT proceedurally generated.

`$summarizer->summarize_text` and `$summarizer->summarize_file` each return a hash-ref containing three array-refs (while `$summarizer->summarize_all` returns a list of these hash-refs):
- **sentences** => a list of full sentences from the given article, with composite scores of the words contained therein

- **fragments** => a list of phrase fragments from the given article, scored as above

- **words**     => a list of all words in the article, scored by a three-factor system consisting of  _frequency of appearance_,  _population standard deviation_, and  _use in important phrase fragments_.

The `$summarizer->pretty_print` method prints a visually pleasing graph of the above three summary categories.

## About Fragments
Phrase fragments are in actuallity short "scraps" of text (usually only two or three words) that are derived from the text via the following process:
1. the entirety of the text is tokenized and scored into a `frequency` table, with a high-pass threshold of frequencies above `# of tokens * user-defined scaling factor`
2. each sentence is tokenized and stored in an array
3. for each word within the `frequency` table, a table of phrase-fragments is derived by finding each occurance of said word and tracking forward and backward by a user-defined "radius" of tokens (defaults to `radius = 5`, does not include the central key-word) — each phrase-fragment is thus compiled of (by default) an 11-token string
4. all fragments for a given key-word are then compared to each other, and each word is deleted if it appears only once amongst all of the fragments
(leaving only <code>_A_ ∪ _B_ ∪ ... ∪ _S_</code> where _A_, _B_,..., _S_ are the phrase-fragments)
5. what remains of each fragment is a list of "scraps" — strings of consecutive tokens — from which the longest scrap is chosen as a representation of the given phrase-fragment
6. when a shorter fragment-scrap is included in the text of a longer scrap (i.e. a different phrase-fragment), the shorter is deleted and its score is added to the score of the longer
7. when multiple fragments are equivalent (i.e. they consist of the same list of tokens when stopwords are excluded), they are condensed into a single scrap in the form of `"(some|word|tokens)"` such that the fragment now represents the tokens of the scrap (excluding stopwords) regardless of order

# AUTHOR

Faelin Landy (CPAN:FaeTheWolf) <faelin.landy@gmail.com>

# COPYRIGHT AND LICENSE

Copyright (C) 2018 by the AUTHOR as listed above

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more details.
