Full_set
-------- 

  Contains tagging for up to 1000 occurrences of each word (fewer where not available) in 
  rounds 2-5. Round 1 contains sense tags for all occurrences in the OANC and MASC (up to ~3500) 
  of each word by one annotator, and 50 occurrences of each word by 2 annotators.

  Full_set Files

      FOLDER     FILES
      word-p --> word-p.txt    : text file (utf-8) with all sentences for 
						  		 word "word" with part of speech "p"  
                 word-p-s.xml  : standoff annotations for sentences in word-p.txt; 
						  		 includes link to original MASC/OANC file
                 word-p-wn.xml : standoff annotations for all Wordnet sense tags

IAA
---

  Contains tagging for 100 (50 in round 2.1) occurrences of each word in rounds 2-5
  by 4-6 taggers, with inter-annotator agreement data.

  IAA_set Files

      FILES
      word-p.txt    		      : text file (utf-8) with all sentences for 
	                                word "word" with part of speech "p"  
      roundX-word-p-sentences.txt : raw tables summarizing the annotations for each word 
				                    in the form expected by Ron Artstein's calculate_alpha.pl perl script

