(deffacts states
	(state initial); initial, will, trust, revision?
	(category NULL);
	(moreFamily NULL); assume they have more family until not
	(moreAssets NULL); keep this NULL to start assets then enter loop if more
	(moreDebts NULL);
	(assetTotal 0);
	(debtTotal 0);
	(entTotal 0)
	(fuzzyTotal 0)
)

;FUZZY TEMPLATES*************************
(deftemplate Affinity
	0 100 units
	((plus2 (75 0) (100 1))
	 (plus1 (60 0)(75 1) (90 0))
	 (zero (25 0) (50 1) (75 0))
	 (minus1 (10 0) (25 1) (40 0))
	 (minus2 (0 1) (25 0))))
	 
(deftemplate Viability
	0 100 units
	((plus2 (75 0) (100 1))
	 (plus1 (60 0)(75 1) (90 0))
	 (zero (25 0) (50 1) (75 0))
	 (minus1 (10 0) (25 1) (40 0))
	 (minus2 (0 1) (25 0))))
	 
(deftemplate Portion
	0 100 units
	((SmlstP (0 1) (25 0))
	 (SmP (0 0) (25 1) (50 0))
	 (MedP (25 0) (50 1) (75 0))
	 (LgP (50 0) (75 1) (100 0))
	 (LgstP (75 0) (100 1))))
	 
;FUZZY TEMPLATE END *************************************

; INITIAL STATE RULE
(defrule rule1
	?f <- (state initial)
	?c <- (category NULL)
	=>
	(printout t "-----"
		    crlf crlf 
		    "Welcome to the expert system on family Wills and Trusts!" crlf
		    "Notes:"crlf
		    "-----"crlf
		    "! Everything is case sensitive, be careful!"crlf
		    "! There is no ammending (so far) unless you start over from scratch!"crlf		    
		    crlf crlf
	)
	
	(printout t "Do you want to make a \"will\" or a \"trust\"?:")
	(assert (category (read)))
	(retract ?f)
	(retract ?c)
	(assert (state one))
	(printout t crlf)
)

(defrule USER_ENTITY
	?f <- (state one)
	=>
	(printout t "Tell me about yourself"crlf 
		     "--------"crlf  
		     "==Help=="crlf
		     "Name: <Your Name>"
		     "Age: <Age> "crlf
		     "Health: <good/fair/poor> Roughly evaluate your health"crlf
			 "Out of State Real Estate: <yes/no>"crlf
			 "Family Situation: <simple/complex/decline>"crlf
		     "--------"crlf) 
	
	(printout t "Name: ")
	(retract ?f)
	(bind ?n (read))
	(assert (name ?n))
	
	(printout t "Age: ")
	(bind ?a (read)) 
	(assert (age ?a))
	
	(printout t "Health: ")
	(bind ?h (read)) 
	(assert (health ?h))
	
	(printout t "Out of State Real Estate: ")
	(bind ?o (read)) 
	(assert (outstate ?o))
	
	(printout t "Family Situation: ")
	(bind ?c (read)) 
	(assert (fam ?c))
	
	(assert (user ?n ?a ?h ?o ?c))
	
	(assert (state onefive))
)

(defrule USER_FILTER
	?a <- (age ?age)
	?h <- (health ?he)
	?o <- (outstate ?os)
	?f <- (fam ?c)
	(test (>= ?age 55))
	(or (str-compare ?he "poor") (str-compare ?os "yes") (str-compare ?c "complex"))
	=>
	(printout t "NOTE: If you are above 55 years in age, have poor health," crlf
		"have out of state assets, and/or have a complex family situation it is" crlf
		"advised that you look more into getting a trust as they will provide you" crlf
		"with more assurance that your assets will be handled according to your more" crlf
		"immediate needs")
	
)

; LINE OF QUESTIONS IF MAKING A WILL
(defrule WILL_ENTITIES
	?f <- (state onefiive)
	?g <- (moreFamily NULL)
	?e <- (entTotal ?et); To total up the amount of potential beneficiaries
	(or (category will) (category trust))
	=>
	(printout t "List all relational entities."crlf 
		     "--------"crlf  
		     "==Help=="crlf
		     "Name: <Family Member Name>"
		     "Relation: <How they relate> choices are:"crlf
		     "{spouse, child, parent, sibling,"crlf
		     "cousin, grandparent, other-family,"crlf
		     "church, other-organization}"crlf
		     "Affinity: <0-100> Desire to give an individual entity a portion of assets"
			 "Viability: <0-100> Financial security and competency of the individual"
			 "enter 'done' for both entries when finished"crlf
			 crlf
		     "--------"crlf) 
	(printout t "Name: ")
	(retract ?g)
	(bind ?n (read))
	(printout t "Relation: ")
	(bind ?r (read)) 
	(printout t "Affinity: ")
	(bind ?a (read)) 
	(printout t "Viability: ")
	(bind ?v (read)) 
	
	(assert (entity ?n ?r ?a ?v))
	
	(retract ?f)
	(assert (state onefive))
	
	(assert (moreFamily yes))
	(printout t )
)


(defrule will1_terminater; this should check if the response from will1 or loop is ever 'done' then break the facts
	(declare (salience 1)); Need this to stop the loop
	?g <- (moreFamily yes)
	?f <- (entity done ? ? ?)
	=>
	(retract ?g)
	(retract ?f)
	(assert (moreFamily no))
	(assert (state two))
)
	
(defrule will1_loop; this should continue taking input if the state is one aand more family is yes.
	?f <- (state one)
	?e <- (entTotal ?et); To total up the amount of potential beneficiaries
	(category will)
	(moreFamily yes)
	=>

	(printout t "------" crlf "Name: ");
	(bind ?n (read))
	(printout t "Relation: ")
	(bind ?r (read))
	(printout t "Affinity: ")
	(bind ?a (read)) 
	(printout t "Viability: ")
	(bind ?v (read))
	
	(assert (entity ?n ?r ?a ?v))
	(assert (entTotal (+ ?et 1)))
	(retract ?e)
	(retract ?f)
	(assert (state onefive))
	
	(printout t )
)

(defrule ASSET_ENTITIES
	?f <- (state two)
	?g <- (moreAssets NULL)
	(category ?)
	=>
	(printout t crlf crlf"List all assets."crlf
			"--------"crlf
			"==Help=="crlf
			"Name: <Asset Name>"crlf
			"Type: <Asset Type> types include:"crlf
			"property, furnishing, valuable, vehicle, savings,"crlf
			"checkings, cash, taxrefund, stocks,"crlf
			"business, other"crlf
			"Value: <Asset Value>"crlf
			crlf
			"--------"crlf
			"Name: ")
	(retract ?g)
	(bind ?n (read))
	(printout t "Type: ")
	(bind ?t (read))
	(printout t "Value: ")
	(bind ?v (read))
	(assert (asset ?n ?t ?v))
	(assert (moreAssets yes))
	(printout t)
)

(defrule asset_total; 		this will go through the assets and add their value to a final asset total 
	;	and reassert assets as final assets meaning their value has already been 
	(declare (salience 1))
	?g <- (asset ?n ?t ?v);	and finalized
	?f <- (assetTotal ?a)
	?m <- (moreAssets no)
	=>
	(retract ?g)
	(retract ?f)
	(bind ?z (+ ?v ?a))
	(printout t ?z)
	(assert (assetTotal ?z))
	(assert (finalAsset ?n ?t ?v))
	(assert (state three))
)

(defrule assetTotalWatch
	(not (asset ? ? ?))
	(moreAssets no)
	=>
	(assert (state three))
)

(defrule asset_loop; this should continue taking input if the state is one aand more family is yes.
	?f <- (state two)
	(category will)
	(moreAssets yes)
	=>
	(printout t "------"crlf)
	(printout t "Name: ")
	(bind ?n (read))
	(printout t "Type: ")
	(bind ?t (read))
	(printout t "Value: ")
	(bind ?v (read))
	(assert (asset ?n ?t ?v))
	(printout t)
	(retract ?f);we must retract and assert this to make sure this rule fires again
	(assert (state two));for a "different" set of facts
)

(defrule asset_terminate
	(declare (salience 1)); Need this to stop the loop
	?i <- (state two)
	?g <- (moreAssets yes)
	?f <- (asset done ? ?)
	=>
	(retract ?g)
	(retract ?f)
	(retract ?i)
	(assert (moreAssets no))
	
)



(defrule DEBT_ENTITIES
	?f <- (state three)
	?g <- (moreDebts NULL)
	(category ?)
	=>
	(printout t "List all debts."crlf
			"--------"crlf
			"==Help=="crlf
			"Name: <Debt Name> crlf"
			"Type: <Debt Type> types include:"crlf
			"student(loans), taxes, support, loan, credit, other"crlf
			"Value: <Debt Value>"crlf
			crlf
			"--------"crlf
			"Name: ")
	(retract ?g)
	(bind ?n (read))
	(printout t "Type: ")
	(bind ?t (read))
	(printout t "Value: ")
	(bind ?v (read))
	(assert (debt ?n ?t ?v))
	(assert (moreDebts yes))
	(printout t)
)

(defrule debt_loop; this should continue taking input if the state is one and more debts is yes.
	?f <- (state three)
	(category will)
	(moreDebts yes)
	=>
	
	(printout  t "--------"crlf"Name: "crlf)
	(bind ?n (read))
	(printout t "Type: "crlf)
	(bind ?t (read))
	(printout t "Value: "crlf)
	(bind ?v (read))
	(assert (debt ?n ?t ?v))
	(retract ?f);we must retract and assert this to make sure this rule fires again
	(assert (state three));for a "different" set of facts
	(printout t)
)

(defrule debt_terminate
	(declare (salience 1)); Need this to stop the loop
	?i <- (state three)
	?g <- (moreDebts yes)
	?f <- (debt done ? ?)
	=>
	(retract ?g)
	(retract ?f)
	(retract ?i)
	(assert (moreDebts no))
	
)

(defrule debt_total; 		this will go through the debts and add their value to a final debt total 
	(declare (salience 1));	and reassert debts as final debts meaning their value has already been 
	?g <- (debt ?n ?t ?v);	and finalized
	?f <- (debtTotal ?a)
	?m <- (moreDebts no)
	=>
	(bind ?z (+ ?v ?a))
	(printout t ?z)
	(assert (debtTotal ?z))
	(assert (finalDebt ?n ?t ?v))
	(retract ?g)
	(retract ?f)
	
)

(defrule debtTotalWatch
	(not (debt ? ? ?))
	(moreDebts no)
	=>
	(assert (state four))
)

(defrule worth_total
	?s <- (state four)
	?a <- (assetTotal ?at)
	?d <- (debtTotal ?dt)
	=>
	(bind ?z (- ?at ?dt))
	(printout t ?z)
	(assert (worthTotal ?z))
	(retract ?s)
	(assert (state five))
)

(defrule over_trust;check to see if the total value of the user 
					;exceeds the limit considered beneficial for a trust
	?w <- (worthTotal ?wt)
	(test(> ?wt 100000))
	=>
	(printout t "Probate costs for wills can cost your beneficiaries between 3-5% of"crlf
		"the estates assets, even 2% of your asset total of: $" ?wt " would be: $" (* ?wt .02)crlf
		"sing a trust may be a good alternative to avoid these probate costs.")
)

	
;DISTRIBUTION RULES *************************
(defrule findAverages
	?s <- (state five)
	?w <- (worthTotal ?wt)
	?m <- (moreFamily no)
	?z <- (fuzzyTotal ?ft)
	?e <- (entTotal ?et)
	=>
	(printout t ?et)
	(assert (fuzzAvg (/ ?ft ?et)))
	(assert (worthAvg (/ ?wt ?et)))
	(retract ?s)
)

(defrule distribute
	?z <- (fuzzedEntity ?n ?r ?a ?v ?t)
	?g <- (fuzzAvg ?fa)
	?w <- (worthAvg ?wa)
	=>
	(printout t ?wa)
	(assert (distEnt ?n ?r (+ ?wa (* ?wa (/ (- ?t ?fa) 100)))));complicated formula to get the portion for
	;person based on the weighted average and the amount of assets
)
	
(defrule testPrint
	?h <- (distEnt ?a ?b ?c)
	=>
	(printout t ?a " " ?b " " ?c)
)
	

	
;FUZZY RULES **************************
(defrule fuzzify
	
	?s <- (state onefive)
	?e <- (entity ?n ?r ?a ?v)
	=>
	(retract ?s)
	(retract ?e)
	(assert (Affinity (?a 0) (?a 1) (?a 0)))
	(assert (Viability (?v 0) (?v 1) (?v 0)))
	(assert (finalEntity ?n ?r ?a ?v))
	(assert (state six))
)

(defrule defuzzify
	(declare (salience -1))
	?e <- (finalEntity ?n ?r ?a ?v)
	?s <- (state six)
	?f <- (Portion ?)
	?z <- (fuzzyTotal ?ft)
	=>
	(bind ?t (moment-defuzzify ?f))
	(retract ?f)
	(retract ?e)
	(retract ?z)
	(assert (fuzzyTotal (+ ?ft ?t)));add the fuzzy value to the total for the averaging for weight
	(assert (fuzzedEntity ?n ?r ?a ?v ?t));add the fuzzy valuation as the 5 (FIFTH) parameter ?t
	(printout t "action --> " ?t crlf)
	(assert (state one)))

(defrule twoTwo
	(Affinity plus2)
	(Viability plus2)
	=>
	(assert (Portion LgstP)))
	
(defrule oneTwo
	(or (and(Affinity plus2)(Viability plus1))
		(and(Affinity plus1) (Viability plus2)))
	=>
	(assert (Portion LgstP)))
	
(defrule zeroTwo
	(or (and (Affinity plus2)(Viability zero))
		(and (Affinity zero) (Viability plus2)))
	=>
	(assert (Portion LgP)))
	
(defrule mOneTwo
	(or (and (Affinity plus2)(Viability minus1))
		(and (Affinity minus1) (Viability plus2)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoTwo
	(or (and (Affinity plus2)(Viability minus2))
		(and (Affinity minus2) (Viability plus2)))
	=>
	(assert (Portion MedP)))
	
(defrule oneOne
	(Affinity plus1)
	(Viability plus1)
	=>
	(assert (Portion LgP)))
	
(defrule zeroOne
	(or (and (Affinity plus1)(Viability zero))
		(and (Affinity zero) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mOneOne
	(or (and (Affinity plus1)(Viability minus1))
		(and (Affinity minus1) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoOne
	(or (and (Affinity plus1)(Viability minus2))
		(and (Affinity minus2) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule zeroZero
	(Affinity zero)
	(Viability zero)
	=>
	(assert (Portion MedP)))
	
(defrule mOneZero
	(or (and (Affinity minus1)(Viability zero))
		(and (Affinity zero) (Viability minus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoZero
	(or (and (Affinity minus2)(Viability zero))
		(and (Affinity zero) (Viability minus2)))
	=>
	(assert (Portion SmP)))
	

(defrule mOneMOne
	(Affinity minus1)
	(Viability minus1)
	=>
	(assert (Portion SmP)))

(defrule mTwoMOne
	(or (and (Affinity minus2)(Viability minus1))
		(and (Affinity minus1) (Viability minus2)))
	=>
	(assert (Portion SmlstP)))
	
	
(defrule mTwoMTwo
	(Affinity minus2)
	(Viability minus2)
	=>
	(assert (Portion SmlstP)))
	
;END FUZZY RULES ********************************************