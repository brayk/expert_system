(deffacts states
	(state initial); intiial, will, trust, revision?
	(category NULL);
	(moreFamily NULL); assume they have more family until not
	(moreAssets NULL); keep this NULL to start assets then enter loop if more
	(moreDebts NULL);
	(assetTotal 0);
	(debtTotal 0);
	(worthTotal 0);
	(entTotal 0)
)

;FUZZY TEMPLATES*************************
(deftemplate Affinity
	0 100 units
	((plus2 (75 0) (100 1))
	 (plus1 (50 0) (100 0))
	 (zero (25 0) (50 1) (75 0))
	 (minus1 (0 1) (50 0))
	 (minus2 (0 1) (25 0))))
	 
(deftemplate Viability
	0 100 units
	((plus2 (75 0) (100 1))
	 (plus1 (50 0) (100 0))
	 (zero (25 0) (50 1) (75 0))
	 (minus1 (0 1) (50 0))
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
	(assert (state one))
	(printout t crlf)
)

; LINE OF QUESTIONS IF MAKING A WILL
(defrule WILL_ENTITIES
	?f <- (state one)
	?g <- (moreFamily NULL)
	?e <- (entTotal ?et); To total up the amount of potential beneficiaries
	(category will)
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
	(assert (entTotal (+ ?et 1)))
	(retract ?e)
	
	(assert (moreFamily yes))
	(printout t )
)


(defrule will1_terminater; this should check if the response from will1 or loop is ever 'done' then break the facts
	(declare (salience 1)); Need this to stop the loop
	?g <- (moreFamily yes)
	?f <- (entity done done done done)
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
	
	(printout t )
	(retract ?f)
	(assert (state one))
)

(defrule ASSET_ENTITIES
	?f <- (state two)
	?g <- (moreAssets NULL)
	(category ?)
	=>
	(printout t crlf crlf"List all assets."crlf
			"--------"crlf
			"==Help=="crlf
			"Name: <Asset Name>"
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
	(assert (asset n? t? v?))
	(assert (moreAssets yes))
	(printout t)
)

(defrule asset_loop; this should continue taking input if the state is one aand more family is yes.
	?f <- (state two)
	(category will)
	(moreAssets yes)
	=>

	(retract ?g)
	(bind ?n (read))
	(printout t "Type: ")
	(bind ?t (read))
	(printout t "Value: ")
	(bind ?v (read))
	(assert (asset n? t? v?))
	(assert (moreAssets yes))
	(printout t)
)

(defrule asset_terminate
	(declare (salience 1)); Need this to stop the loop
	?g <- (moreAssets yes)
	?f <- (asset done done done)
	=>
	(retract ?g)
	(retract ?f)
	(assert (moreAssets no))
	(assert (state three))
)

(defrule asset_total; 		this will go through the assets and add their value to a final asset total 
	(declare (salience 1));	and reassert assets as final assets meaning their value has already been 
	?g <- (asset ?n ?t ?v);	and finalized
	?f <- (assetTotal ?a)
	=>
	(assert (assetTotal (+ ?a ?v)))
	(assert (finalAsset ?n ?t ?v))
	(retract ?g)
	(retract ?f)
)

(defrule DEBT_ENTITIES
	?f <- (state three)
	?g <- (moreDebts NULL)
	(category ?)
	=>
	(printout t "List all debts."crlf
			"--------"crlf
			"==Help=="crlf
			"Name: <Debt Name>"
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
	(assert (debt n? t? v?))
	(assert (moreDebts yes))
	(printout t)
)

(defrule debt_loop; this should continue taking input if the state is one and more debts is yes.
	?f <- (state three)
	(category will)
	(moreDebts yes)
	=>
	(retract ?g)
	(bind ?n (read))
	(printout t "Type: ")
	(bind ?t (read))
	(printout t "Value: ")
	(bind ?v (read))
	(assert (debt n? t? v?))
	(assert (moreDebts yes))
	(printout t)
)

(defrule debt_terminate
	(declare (salience 1)); Need this to stop the loop
	?g <- (moreDebt yes)
	?f <- (debt done done done)
	=>
	(retract ?g)
	(retract ?f)
	(assert (moreDebts no))
	(assert (state four))
)

(defrule debt_total; 		this will go through the debts and add their value to a final debt total 
	(declare (salience 1));	and reassert debts as final debts meaning their value has already been 
	?g <- (debt ?n ?t ?v);	and finalized
	?f <- (debtTotal ?a)
	=>
	(assert (debtTotal (+ ?a ?v)))
	(assert (finalDebt ?n ?t ?v))
	(retract ?g)
	(retract ?f)
)

(defrule worth_total
	?s <- (state four)
	?a <- (assetTotal ?at)
	?d <- (debtTotal ?dt)
	=>
	(assert (worthTotal (- at dt)))
	(retract ?s)
	(assert (state five))
)

	
	
	

	
;FUZZY RULES **************************
(defrule fuzzify
	?s <- (state five)
	?e <- (entity ?n ?r ?a ?v)
	=>
	(assert (Affinity (?a 0) (?a 1) (?a 0)))
	(assert (Viability (?a 0) (?a 1) (?a 0)))
	(retract ?s)
	(assert (state six))
)

(defrule defuzzify
	(declare (salience -1))
	?s <- (state six)
	?f <- (Portion ?)
	=>
	(bind ?t (moment-defuzzify ?f))
	(printout t "action --> " ?t crlf))
	(assert (state five))
)

(defrule twoTwo
	(Affinity plus2)
	(Viabilityy plus2)
	=>
	(assert (Portion LgstP)))
	
(defrule oneTwo
	(or ((Affinity plus2)(Viability plus1))
		((Affinity plus1) (Viability plus2)))
	=>
	(assert (Portion LgstP)))
	
(defrule zeroTwo
	(or ((Affinity plus2)(Viability zero))
		((Affinity zero) (Viability plus2)))
	=>
	(assert (Portion LgP)))
	
(defrule mOneTwo
	(or ((Affinity plus2)(Viability minus1))
		((Affinity minus1) (Viability plus2)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoTwo
	(or ((Affinity plus2)(Viability minus2))
		((Affinity minus2) (Viability plus2)))
	=>
	(assert (Portion MedP)))
	
(defrule oneOne
	(Affinity plus1)
	(Viabilityy plus1)
	=>
	(assert (Portion LgP)))
	
(defrule zeroOne
	(or ((Affinity plus1)(Viability zero))
		((Affinity zero) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mOneOne
	(or ((Affinity plus1)(Viability minus1))
		((Affinity minus1) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoOne
	(or ((Affinity plus1)(Viability minus2))
		((Affinity minus2) (Viability plus1)))
	=>
	(assert (Portion MedP)))
	
(defrule zeroZero
	(Affinity zero)
	(Viabilityy zero)
	=>
	(assert (Portion MedP)))
	
(defrule mOneZero
	(or ((Affinity minus1)(Viability zero))
		((Affinity zero) (Viability minus1)))
	=>
	(assert (Portion MedP)))
	
(defrule mTwoZero
	(or ((Affinity minus2)(Viability zero))
		((Affinity zero) (Viability minus2)))
	=>
	(assert (Portion SmP)))
	

(defrule mOneMOne
	(Affinity minus1)
	(Viabilityy minus1)
	=>
	(assert (Portion SmP)))

(defrule mTwoMOne
	(or ((Affinity minus2)(Viability minus1))
		((Affinity minus1) (Viability minus2)))
	=>
	(assert (Portion SmlstP)))
	
	
(defrule mTwoMTwo
	(Affinity minus2)
	(Viabilityy minus2)
	=>
	(assert (Portion SmlstP)))
	