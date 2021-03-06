;;; Four category conjunctive soybean data in form for COBWEB (first is name for instance)

(setf *domains* 
 `((0 1 2 3 4 5 6) (0 1) (0 1 2) (0 1 2) (0 1) (0 1 2 3) (0 1 2 3) (0 1 2) 
   (0 1 2) (0 1 2) (0 1) (0 1) (0 1 2) (0 1 2) (0 1 2) (0 1) (0 1) (0 1 2) (0 1) (0 1)
   (0 1 2 3) (0 1 2 3) (0 1) (0 1 2) (0 1) (0 1 2) (0 1) (0 1 2 3) (0 1 2 3 4) (0 1) 
   (0 1) (0 1) (0 1) (0 1) (0 1 2)))

(setf *feature-names*
      '(TIME-OF-OCCURRENCE PLANT-STAND PRECIPITATION TEMPERATURE OCCURENCE-OF-HAIL CROPPING-HISTORY
        DAMAGED-AREA SEVERITY SEED-TREATMENT SEED-GERMINATION PLANT-HEIGHT CONDITION-OF-LEAVES
	YELLOW-LEAF-SPOT-HALOS LEAF-SPOT-MARGINS LEAF-SPOT-SIZE SHOT-HOLING LEAF-MALFORMATION
	LEAF-MILDEW-GROWTH CONDITION-OF-STEM STEM-LODGING STEM-CANKERS CANKER-LESION-COLOR
	FRUITING-BODIES-ON-STEM EXTERNAL-DECAY-OF-STEM MYCELIUM-ON-STEM INTERNAL-DISCOLORATION-OF-STEM
	SCLEROTIA-INTERNAL-OR-EXTERNAL CONDITION-OF-FRUIT-PODS FRUIT-SPOTS CONDITION-OF-SEED
	SEED-MOLD-GROWTH SEED-DISCOLORATION SEED-SIZE SEED-SHRIVELING CONDITION-OF-ROOTS))

(setf *raw-examples*
     '((CHAR0006 (3 0 0 2 1 0 2 1 0 1 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (DIAP0009 (5 0 2 1 0 3 1 1 1 2 1 1 0 2 2 0 0 0 1 1 3 0 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (CHAR0009 (4 0 0 1 0 2 3 1 1 1 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (DIAP0006 (4 0 2 1 0 3 0 2 0 2 1 1 0 2 2 0 0 0 1 0 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (DIAP0005 (5 0 2 1 0 2 0 1 1 0 1 1 0 2 2 0 0 0 1 1 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (PHYT0005 (1 1 2 0 0 3 1 1 1 2 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (DIAP0004 (3 0 2 1 0 2 1 1 0 1 1 1 0 2 2 0 0 0 1 1 3 0 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (PHYT0017 (3 1 1 0 0 2 1 2 1 2 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0016 (0 1 2 1 1 1 1 1 0 0 1 1 0 2 2 0 0 0 1 0 1 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0015 (1 1 2 1 1 3 1 2 0 1 1 1 0 2 2 0 0 0 1 1 1 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0002 (2 1 2 0 0 1 1 2 0 0 1 1 0 2 2 0 0 0 1 0 1 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0001 (3 1 2 0 0 2 1 2 1 1 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (DIAP0010 (4 0 2 1 1 1 0 1 0 2 1 1 0 2 2 0 0 0 1 0 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (RHIZ0009 (4 0 2 0 1 0 1 2 0 2 1 1 0 2 2 0 0 0 1 1 1 1 0 1 1 0 0 3 4 0 0 0 0 0 0))
       (PHYT0004 (1 1 2 1 0 0 1 2 1 1 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (DIAP0003 (3 0 2 1 0 1 0 2 1 2 1 1 0 2 2 0 0 0 1 0 3 0 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (CHAR0001 (5 0 0 2 1 3 3 1 1 2 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (CHAR0004 (3 0 0 1 0 1 2 1 0 0 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (PHYT0007 (3 1 2 0 0 1 1 2 1 0 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (CHAR0008 (5 0 0 2 0 3 2 1 0 2 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (PHYT0014 (1 1 2 0 0 0 1 2 1 0 1 1 0 2 2 0 0 0 1 0 2 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (RHIZ0004 (0 1 2 0 0 1 1 1 1 1 1 0 0 2 2 0 0 0 1 0 1 1 0 1 1 0 0 3 4 0 0 0 0 0 0))
       (PHYT0012 (2 1 1 0 0 3 1 2 0 2 1 1 0 2 2 0 0 0 1 0 1 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (RHIZ0010 (0 1 2 0 0 2 1 1 1 1 1 0 0 2 2 0 0 0 1 0 1 1 0 1 0 0 0 3 4 0 0 0 0 0 0))
       (RHIZ0007 (0 1 2 0 0 0 1 1 0 1 1 0 0 2 2 0 0 0 1 0 1 1 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0009 (0 1 1 1 0 1 1 1 0 0 1 1 0 2 2 0 0 0 1 0 1 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (CHAR0010 (6 0 0 2 1 0 2 1 0 0 1 1 0 2 2 0 0 0 1 1 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (RHIZ0003 (2 1 2 0 0 3 1 2 0 1 1 0 0 2 2 0 0 0 1 0 1 1 0 1 0 0 0 3 4 0 0 0 0 0 0))
       (RHIZ0001 (0 1 2 0 0 0 1 1 1 2 1 0 0 2 2 0 0 0 1 0 1 1 0 1 0 0 0 3 4 0 0 0 0 0 0))
       (CHAR0007 (6 0 0 1 1 3 3 1 1 0 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (PHYT0008 (2 1 2 1 1 3 1 2 1 2 1 1 0 2 2 0 0 0 1 0 2 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (PHYT0013 (1 1 2 1 1 2 3 1 1 1 1 1 0 2 2 0 0 0 1 0 2 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (RHIZ0005 (0 1 2 0 0 1 1 2 1 2 1 0 0 2 2 0 0 0 1 0 1 1 0 1 0 0 0 3 4 0 0 0 0 0 0))
       (PHYT0006 (2 1 2 1 1 1 1 2 0 2 1 1 0 2 2 0 0 0 1 0 1 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (CHAR0003 (5 0 0 2 1 2 2 1 0 2 1 1 0 2 2 0 0 0 1 1 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (CHAR0002 (6 0 0 2 0 1 3 1 1 0 1 1 0 2 2 0 0 0 1 0 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       (DIAP0001 (6 0 2 1 0 1 0 1 0 2 1 1 0 2 2 0 0 0 1 0 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (RHIZ0006 (3 0 2 0 1 3 1 2 0 1 1 0 0 2 2 0 0 0 1 1 1 1 0 1 1 0 0 3 4 0 0 0 0 0 0))
       (DIAP0007 (6 0 2 1 0 1 1 1 0 0 1 1 0 2 2 0 0 0 1 1 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (PHYT0011 (0 1 1 1 1 2 1 2 1 0 1 1 0 2 2 0 0 0 1 1 2 2 0 1 0 0 0 3 4 0 0 0 0 0 1))
       (RHIZ0008 (2 1 2 0 0 3 1 2 0 2 1 0 0 2 2 0 0 0 1 0 1 1 0 1 1 0 0 3 4 0 0 0 0 0 0))
       (PHYT0003 (0 1 2 1 0 3 1 1 0 0 1 1 0 2 2 0 0 0 1 0 1 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (RHIZ0002 (2 1 2 0 0 2 1 1 0 2 1 0 0 2 2 0 0 0 1 0 1 1 0 1 1 0 0 3 4 0 0 0 0 0 0))
       (PHYT0010 (0 1 2 1 0 3 1 1 0 2 1 1 0 2 2 0 0 0 1 0 1 2 0 0 0 0 0 3 4 0 0 0 0 0 1))
       (DIAP0008 (3 0 2 1 0 2 0 2 1 1 1 1 0 2 2 0 0 0 1 0 3 0 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (DIAP0002 (6 0 2 1 0 3 0 1 1 1 1 1 0 2 2 0 0 0 1 0 3 1 1 1 0 0 0 0 4 0 0 0 0 0 0))
       (CHAR0005 (4 0 0 1 1 1 3 1 1 1 1 1 0 2 2 0 0 0 1 1 0 3 0 0 0 2 1 0 4 0 0 0 0 0 0))
       ))


