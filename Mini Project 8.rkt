#lang racket
(require csc151)
(require csc151/rex)
(require rackunit)
(require rackunit/text-ui)

;;; CSC-151 - Rebelsky
;;; Mini Project 8: Sentence and Word Analysis
;;; Date: November 19 - December 5
;;; Names: Elliot Swaim, Jules Wood, Matilda Carne, Nifemi Ogunmesa
;;; Acknowledgements:
;;;   - Verb forms dictionary taken from https://github.com/monolithpl/verb.forms.dictionary

#| STRUCTS FOR PARAGRAPHS, SENTENCES, WORDS, AND conjuclentions |#

#| Conjugation Struct |#

;;; (conjugation person number tense) -> conjugation
;;;   person : 'first, 'second, or 'third
;;;   number : 'singular or 'plural
;;;   tense : 'past or 'present
;;; Stores information about a verb's conjugation
(struct conjugation-kernel (person number tense))
(define conjugation
  (lambda (person number tense)
    (cond
      [(not (or (equal? "first" person)
                (equal? "second" person)
                (equal? "third" person)))
       (error "conjugation : person" person)]
      [(not (or (equal? "singular" number)
                (equal? "plural" number)))
       (error "conjugation : number" number)]
      [(not (or (equal? "past" tense)
                (equal? "present" tense)))
       (error "conjugation : tense" tense)]
      [else
       (conjugation-kernel person number tense)])))

(define sample-con (conjugation "first" "singular" "present"))
(test-true "sample conjugation" (conjugation-kernel? sample-con))




#| Declention Struct |#

;;; (declention number gender case) -> declention?
;;;   number : 'singular or 'plural
;;;   gender : 'masculine, 'feminine, or 'neuter (default is 'neuter)
;;;   case : 'nominative, 'objective, or 'possessive (default is 'nominative)
;;; Stores information about a noun's (or pronoun's) declention
;;; Note:
;;;   * Nominative case is for subjects and complements
;;;     - "She ate the cake."
;;;     - "The person who ate the cake is she."
;;;   * Objective is used direct objects, indirect objects, and objects of preposition
;;;     - "I bought them."
;;;     - "I bought them a cake."
;;;     - "I bought a cake for them."
;;;   * Possessive case is used for possession
;;;     - "That's his cake."
;;;     - "That cake is his."
(struct declention-kernel (number gender case))

(define declention
  (lambda (number gender case)
    (cond
      [(not (or (equal? "singular" number)
                (equal? "plural" number)))
       (error "declention : number" number)]
      [(not (or (equal? "male" gender)
                (equal? "female" gender)))
       (error "declention : gender" gender)]
      [(not (or (equal? "possessive" case)
                (equal? "objective" case)))
       (error "declention : case" case)]
      [else
       (declention-kernel number gender case)])))

(define sample-dec (declention "singular" "female" "possessive"))
(test-true "sample declention" (declention-kernel? sample-dec))




#| Conjuclention Struct |#

;;; (conjuclention part-of-speech conjucline) -> conjuclention?
;;;   part-of-speech : one of:
;;;       'noun, 'verb, 'adjective, 'adverb, 'preposition, 'conjunction, 'interjection, or #f
;;;       (pronouns count as nouns)
;;;   conjucline : declention? or conjugation? or #f
;;; Stores information about a words part of speech and conjugation or declention
(struct conjuclention-kernel (part-of-speech conjucline))

(define conjuclention
  (lambda (part-of-speech conjucline)
    (cond
      [(not (or (equal? "verb" part-of-speech)
                (equal? "noun" part-of-speech)))
       (error "conjuclention: part-of-speech contract violation.  Given: " part-of-speech)]
      [(not (or (declention-kernel? conjucline)
                (conjugation-kernel? conjucline)))
       (error "conjuclention : conjucline contract violation.  Given: " conjucline)] ; hitting an error on this.....
      [else
       (conjuclention-kernel part-of-speech conjucline)])))



(define sample-conjuc-verb (conjuclention "verb" (conjugation "third" "plural" "past")))
;(test-true "conjuclention verb" (conjuclention-kernel? sample-conjuc-verb))





#| Word Struct |#

;;; (word str conjuclention) -> word?
;;;   str : string?
;;;   conjuclention : conjuclention?
;;; Creates a word struct
(struct word-kernel (str conjuclention))

(define word
  (lambda (wordstr conjuclention)
    (cond
      [(not (string? wordstr))
       (error "word : expected string" wordstr)]
      [(not (conjuclention-kernel? conjuclention))
       (error "word : expected conjuclention" conjuclention)]
      [else
       (word-kernel wordstr conjuclention)])))
; (word "cat" (conjuclention "noun" (declention "singular" "female" "objective")))
;;; not currently working but it will. 




#| Sentence Struct |#

;;; (sentence-kernel words) -> sentence?
;;;   words : list? of word?
;;; Struct for storing information about each sentence

(struct sentence-kernel (words))

;;;/// each member of list should be str
(define sentence
  (lambda (sentences)
    (cond
      [(not (list? sentences))
       (error "sentence not words")]
      [else
       (sentence-kernel sentences)])))




#| Paragraph Struct |#

;;; (paragraph-kernel sentences) -> paragraph?
;;;   sentences: list? of sentences?
;;; Struct for storing information about each paragraph

(struct paragraph-kernel (sentences))

;;; /// check if each element in list should be string . fix later
(define paragraph
  (lambda (sentences)
    (cond
      [(not (list? sentences))
       (error "paragraph not sentences")]
      [else
       (sentences)])))



#| IDENTIFYING PARAGRAPHS, SENTENCES, AND WORDS |#

;;; (string->conjuclention str) -> conjuclention?
;;;   str : string?
;;; Analyses the word and returns the appropriate conjuclention struct
;;; NOTE: we won't actually analyze the word until part two.  For now, just
;;;  make it give a default value, such as clasifying all words as singular,
;;;  neuter, nominative nouns.
;;; TODO: Make this actually word (during part two)
(define string->conjuclention
  (lambda (str)
    (declention 'singular 'neuter 'nominative)))

;;; (string->word str) -> word?
;;;    str : string?
;;; Converts a string to a word struct.
;;;  Assumes str contains one word
(define string->word
  (lambda (str)
    (word str
          (string->conjuclention str))))


;;; (string->words-list str) -> list? of string?
;;;   str : string?
;;; Creates a list of words represented by strings
(define string->words-list
  (let* ([rex-alphabetical (rex-any-of (rex-char-range #\a #\z)
                                       (rex-char-range #\A #\Z))]
         [rex-contractions (rex-concat (rex-repeat rex-alphabetical)
                                       (rex-char-set "'")
                                       (rex-repeat rex-alphabetical))])
    (lambda (str)
      (rex-find-matches (rex-any-of rex-contractions
                                    (rex-repeat rex-alphabetical))
                        str))))

(test-equal? "Empty string"
             (string->words-list "")
             '())
(test-equal? "Hyphens, Contractions, and Punctuation"
             (string->words-list "Hello")
             '("Hello"))
(test-equal? "Hyphens, Contractions, and Punctuation"
             (string->words-list "Test test-ing nope- -not don't 'try this' Punctuation? Not.Fun")
             '("Test" "test" "ing" "nope" "not" "don't" "try" "this" "Punctuation" "Not" "Fun"))

;;; (string->sentences-list str) -> list? of string?
;;;   str : string?
;;; Breaks apart a string into a list of sentences contained within the string
;;; Includes the end punctuation with each sentence (including new line characters)
;;; If str does not end in any punctuation, it treats the end of str as a new line character
(define string->sentences-list
  (lambda (str)
    (let ([endmarks ".?\n"])
      (rex-find-matches (rex-concat (rex-char-antiset (string-append endmarks
                                                                     " \t\n\r"))
                                    (rex-repeat (rex-char-antiset endmarks))
                                    (rex-char-set endmarks))
                        (string-append str
                                       "\n")))))

(test-equal? "string->sentences-list: Empty string"
             (string->sentences-list "")
             '())
(test-equal? "string->sentences-list: Single sentence"
             (string->sentences-list "Hello, how are you?")
             '("Hello, how are you?"))
(test-equal? "string->sentences-list: Multiple sentences"
             (string->sentences-list "I'm doing good.  How about you?  Are you doing good?")
             '("I'm doing good." "How about you?" "Are you doing good?"))
(test-equal? "string->sentences-list: New line"
             (string->sentences-list "First thing\nSecond thing\nThird thing")
             '("First thing\n" "Second thing\n" "Third thing\n"))
(test-equal? "string->sentences-list: Single sentence without punctuation"
             (string->sentences-list "I'm going to test this sentence")
             '("I'm going to test this sentence\n"))

;;; (string->paragraph-list str) -> list-of-string?
;;;    str : s(test-equal? "a singular paragraph"
;;; breaks apart string into its paragraphs
;;; if the strings ends without any new line it adds two new lines
(define string->paragraph-list
  (lambda (str)
    (let* ([parsep1 "\n\n"]
           [parsep2 "\n\t"]
           [rex-letter (rex-repeat-0 (rex-char-antiset "\n"))])
      (map string-trim (rex-find-matches (rex-concat rex-letter
                                                     (rex-any-of (rex-string "\n")
                                                                 rex-letter)
                                                     (rex-repeat (rex-char-antiset parsep2))
                                                     (rex-any-of (rex-string parsep1)
                                                                 (rex-string parsep2)))
                                         (string-append str
                                                        parsep1))))))

(test-equal? "a singular paragraph"
             (string->paragraph-list "i'm a little paragraph. ok.")
             '("i'm a little paragraph. ok."))
(test-equal? "multiple paragraphs"
             (string->paragraph-list "i'm a little paragraph\n\n ok.\n\t I like cs.")
             '("i'm a little paragraph" "ok." "I like cs."))
(test-equal? "empty string"
             (string->paragraph-list "")
             '())
(test-equal? "single tab"
             (string->paragraph-list "I'm a little tea pot.\t short and stout")
             '("I'm a little tea pot.\t short and stout"))
(test-equal? "single new-line"
             (string->paragraph-list "I'm a little tea pot.\n short and stout")
             '("I'm a little tea pot.\n short and stout"))                                    

;;; (string->sentence str) -> sentence?
;;;   str : string?
;;; Converts a string into a sentence struct.
;;;  Assumes str contains one sentence
(define string->sentence
  (lambda (str)
    (sentence (map string->word (string->words-list str)))))

;;; (string->paragraph str) -> paragraph?
;;;   str : string?
;;; Converts a string to a paragraph struct.
;;;  Assumes str contains only one paragraph.
(define string->paragraph
  (lambda (str)
    (paragraph (map string->sentence (string->sentences-list str)))))

;;; (file->paragraph filename) -> paragraph?
;;;   filename : string? that is a valid text file name
;;; Converts the text in a document into a paragraph struct.
;;;  Assumes file contains only one paragraph.
(define file->paragraph
  (lambda (filename)
    (string->paragraph (file->string filename))))

;;; (string->paragraphs str) -> list? of paragraph?
;;;   str : string?
;;; Creates a list of paragraphs contained within the string
(define string->paragraphs
  (lambda (str)
    (map paragraph (string->paragraph-list str))))


;;; (file->paragraphs filename) -> list? of paragraph?
;;;   filename : string? that is a valid text file name
;;; Creates a list of paragraphs contained within the given text document
(define file->paragraphs
  (lambda (filename)
    (string->paragraphs (file->string filename))))


#| WORD ANALYSIS |#
;;; (csv->column-list filename column-num) -> list? of string?
;;;   filename : string? that is a valid file name
;;;   column-num : integer?
;;; Returns a list of the values in the given column
;;; column-num = 0 gives the first column
(define csv->column-list
  (lambda (filename column-num)
    (map (lambda (line)
           (string-trim (list-ref (string-split line #px"[\t,]" #:trim? #f) column-num)))
         (file->lines filename))))

;;; (file->verb-dictioary filename)
;;;   filename : string? that is a valid file name
;;; filename should be formatted as follows
;;;   base1,present_simple1,past_simple1,past_participle1,present_participle1   
;;;   base2,present_simple2,past_simple2,past_participle2,present_participle2   
;;;   ...
;;; Dictionay hash will be formatted as follows
;;;   '#hash(
;;;     (base . (...list_of_base_verbs...)) ; (base form is infinitive and 1st/2nd present smple)
;;;     (present-simple . (...list_of_present_simple_verbs...)) ; (only 3rd person)
;;;     (past-simple . (...list_of_past_simple_verbs...))
;;;     (past-participle . (...list_of_past_participle_verbs...))
;;;     (present-participle . (...list_of_present_participle_verbs...)))
(define file->verb-dictionary
  (lambda (filename)
    (hash 'base (csv->column-list filename 0)
          'present-simple (csv->column-list filename 1)
          'past-simple (csv->column-list filename 2)          
          'past-participle (csv->column-list filename 3)
          'present-participle (csv->column-list filename 4))))




;;; (file->noun-dictionary filename) -> hash?
;;;   filename : string? that is a valid file name
;;; filename should be formatted as follows
;;;   word1_singular,word1_plural
;;;   word2_singular,word2_plural
;;;   ...
;;; (e.g. https://raw.githubusercontent.com/djstrong/nouns-with-plurals/master/noun.csv)
;;; Dictionary hash  will be formatted as follows
;;;   '#hash(
;;;     (singular . (...list_of_singular_nouns...)
;;;     (plural . (...list_of_plural_nouns...))))
(define file->noun-dictionary
  (lambda (filename)
    (hash 'singular (csv->column-list filename 0)
          'plural (csv->column-list filename 1))))



;;; (file->pronouns-dictionary personal-pronouns other-pronouns) -> hash?
;;;   personal-pronouns : string? that is a valid file name
;;;   other-pronouns : string? that is a valid file name
;;; personal-pronouns should be formatted as follows
;;;   i,me,my,mine,myself
;;;   we,us,out,ours,ourself
;;;   thou,thee,thy,thine,thineself
;;;   you,you,your,yours,yourself
;;;   ye,you,your,yours,yourself
;;;   he,him,his,his,himself
;;;   she,her,her,hers,herself
;;;   it,it,its,--,itself
;;;   they,them,their,theirs,themselves
;;; other-pronouns should be formatted as follows
;;;   indefinite1,indefinite2,...
;;;   demonstrative1,demonstrative2,...
;;;   interrogative1,interrogative2,...
;;;   relative1,relative2,...
;;; Dictioary hash will be formatted as follows
;;;   '#hash(
;;;     (1S . (nominative objective possessive-adjective possessive reflexive)) ; (1st singular)
;;;     (1P . (nominative objective possessive-adjective possessive reflexive)) ; (1st plural)
;;;     (2AS . (nominative objective possessive-adjective possessive reflexive)) ; (2nd archaic singular) 
;;;     (2S . (nominative objective possessive-adjective possessive reflexive)) ; (2nd singular)
;;;     (2P . (nominative objective possessive-adjective possessive reflexive)) ; (2nd plural)
;;;     (3M . (nominative objective possessive-adjective possessive reflexive)) ; (3rd masculine singular)
;;;     (3F . (nominative objective possessive-adjective possessive reflexive)) ; (3rd feminine singular)
;;;     (3N . (nominative objective possessive-adjective possessive reflexive)) ; (3rd neuter singular)
;;;     (3P . (nominative objective possessive-adjective possessive reflexive)) ; (3rd plural)
;;;     (indefinite . (...list_of_indefinite_pronouns...))
;;;     (demonstrative . (...list_of_demonstrative_pronouns...))
;;;     (interrogative . (...list_of_interrogative_pronouns...))
;;;     (relative . (...list_of_relative_pronouns...)))




;;; (file->other-dictionary adjectives adverbs prepositions cojuctions interjections)
;;;    all parameters : string? that is a valid file name
;;; All files should be formatted as a new-line separated list of words
;;; TODO Add articles (the, an, a) to adjective list
;;; Dictionary hash will be formatted as follows
;;;   '#hash(
;;;     (adjective . (...list_of_adjectives...))
;;;     (adverb . (...list_of_adverbs...))
;;;     (preposition . (...list_of_prepositions...))
;;;     (interjection . (...list_of_interjectios...)))




;;; (string->noun str dictionary) -> word? with noun properties
;;;   str : string? that is one word
;;;   dictionary : hash? formatted as noun dictionary
;;; If the given word is in the noun dictionary, it returns a word
;;;  struct with noun properties.  Othwerise, it returns #f



;;; (string->verb str dictionary)
;;;
;;;




;;; (string->pronoun str dictionary)
;;;
;;;




;;; TODO Rewrite string->word to work using the above functions as well as the other words dictionary
#| OPTIONAL: SENTENCE ANALYSIS |#
