;;; Sample COBWEB animal data taken from the ML article

(setf *feature-names* '(body-cover heart-chamber body-temp fertilization))

(setf *domains* '((hair feathers cornified-skin moist-skin scales)
		  (two three imperfect-four four)
		  (regulated unregulated)
		  (internal external)))

(setf *raw-examples*
      '(
	(amphibian (moist-skin three unregulated external))
	(fish (scales two unregulated external))
	(mammal (hair four regulated internal))
	(bird   (feathers four regulated internal)) 
	(reptile (cornified-skin imperfect-four unregulated internal))
	(fish2 (scales two unregulated external))
      ))
