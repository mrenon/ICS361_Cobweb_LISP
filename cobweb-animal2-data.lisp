;;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER; Base: 10 -*-
;;;; Name:  Mauricio Renon            Date: November 6,2014
;;;; Course: ICS361                   Assignment: 4
;;;; File name:  cobweb-animal2-data.lisp  

;;; Additional sample animal data for COBWEB

(setf *feature-names* '(body-cover air water mouth birth appendage color place feet))

(setf *domains* 
      '((hair feathers) (fly no-fly) (no-swim swim) (teeth beak) (live-birth eggs) (legs wings)
	(brown white tawny black red-breast tuxedo) (west ut-campus africa midwest antartic)
	(no-web webbed)))




(setf *raw-examples*
      '(
	(Penguin   (feathers  no-fly swim    beak   eggs         wings  tuxedo      Antartic  webbed))
	(Robin     (feathers  fly    no-swim beak   eggs         wings  red-breast  Midwest   no-web))
	(Panther   (hair      no-fly no-swim teeth  live-birth   legs   black       Africa    no-web))
	(Grackle   (feathers  fly    no-swim beak   eggs         wings  black       UT-Campus no-web))
	(Lion      (hair      no-fly no-swim teeth  live-birth   legs   tawny       Africa    no-web))
	(Eagle     (feathers  fly    no-swim beak   eggs         wings  white       West      no-web))
	(Bear      (hair      no-fly no-swim teeth  live-birth   legs   black       West      no-web))
	(Duck      (feathers  fly    swim    beak   eggs         wings  brown       Midwest   webbed))
	
    ;;this is the new instances that i created
	(Rat       (hair      no-fly no-swim teeth  live-birth   legs   brown       West      no-web))
	(Ox        (hair      no-fly no-swim teeth  live-birth   legs   black       Midwest   no-web)) 
	(Tiger     (hair      no-fly no-swim teeth  live-birth   legs   white       Africa    no-web))
	(Rabbit    (hair      no-fly no-swim teeth  live-birth   legs   white       Midwest   no-web))
	(Dragon    (hair      fly    swim    teeth  eggs         legs   black       Midwest   no-web))
    ;;missing snake LOL
	(Horse     (hair      no-fly no-swim teeth  live-birth   legs   brown       West      no-web))
	(Goat      (hair      no-fly no-swim teeth  live-birth   legs   white       Midwest   no-web))
	(Monkey    (hair      no-fly no-swim teeth  live-birth   legs   brown       Africa    no-web))
	(Rooster   (hair      fly    no-swim beak   eggs         wings  white       Midwest   no-web))
	(Dog       (hair      no-fly no-swim teeth  live-birth   legs   black       West      no-web))
	(Pig       (hair      no-fly no-swim teeth  live-birth   legs   white       West      no-web))
       ))

