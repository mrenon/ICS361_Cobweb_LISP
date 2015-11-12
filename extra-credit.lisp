;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name:  Mauricio Renon            Date: November 6,2014
;;;; Course: ICS361                   Assignment: 4
;;;; File name:  extra-credit.lisp    (was header.lisp)

(in-package :User) ; optional - this is also in the top line above


;;; Additional sample animal data for COBWEB

(setf *feature-names* '(primary-type secondary-type starter did-ash-catch 
	number-of-evolutions mega-evolve uses-stone battle-tier))

(setf *domains* 
      '((normal fire fighting water flying grass poison electric ground psychic
      	rock ice bug dragon ghost dark steel fairy) (normal fire fighting water flying grass poison electric ground psychic
      	rock ice bug dragon ghost dark steel fairy none) (yes no) (yes no) (one two three) (yes no) (yes no)
	(uber overused banned underused rarelyused neverused littlecup)))

(setf *raw-examples*
      '(
	(pikachu    (electric none    no  yes three no  yes neverused)) ;1
	(squirtle   (water    none    yes yes three yes no  littlecup)) ;2
	(charizard  (fire     flying  yes yes three yes no  neverused)) ;3
	(mewtwo     (psychic  none    no  no  one   yes no  uber))      ;4
	(bulbasaur  (grass    poison  yes yes three yes no  littlecup)) ;5
	(jirachi    (psychic  steel   no  no  one   no  no  overused))  ;6
	(chansey    (normal   none    no  no  three no  no  overused))  ;7
	(dragonite  (dragon   flying  no  no  three no  no  overused))  ;8
	(metagross  (psychic  steel   no  no  three yes no  underused)) ;9
	(beedrill   (bug      poison  no  no  three yes no  neverused)) ;10

	(slowbro    (psychic  water   no  no  three yes yes overused))  ;11
	(mudkip     (water    none    yes no  three yes no  littlecup)) ;12
	(exploud    (normal   none    no  no  three no  no  rarelyused));13
	(tropius    (flying   grass   no  no  one   no  no  neverused)) ;14
	(magikarp   (water    none    no  no  two   yes no  littlecup)) ;15
       ))

