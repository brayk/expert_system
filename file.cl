(deffacts states
	(state initial); intiial, will, trust, revision?
	(category NULL);
	(moreFamily NULL); assume they have more family until not
	(moreAssets NULL); keep this NULL to start assets then enter loop if more
	(moreDebts NULL);
	(assetTotal 0);
	(debtTotal 0);
	(worthTotal 0);
)


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
		     "enter 'done' for both entries when finished"crlf
			 crlf
		     "--------"crlf 
		     "Name: ")
	(retract ?g)
	(bind ?n (read))
	(printout t "Relation: ")
	(bind ?r (read)) 
	(assert (entity ?n ?r))
	(assert (moreFamily yes))
	(printout t )
)


(defrule will1_terminater; this should check if the respsonse from will1 or loop is ever 'done' then break the facts
	(declare (salience 1)); Need this to stop the loop
	?g <- (moreFamily yes)
	?f <- (entity done done)
	=>
	(retract ?g)
	(retract ?f)
	(assert (moreFamily no))
	(assert (state two))
)
	
(defrule will1_loop; this should continue taking input if the state is one aand more family is yes.
	?f <- (state one)
	(category will)
	(moreFamily yes)
	=>

	(printout t "------" crlf "Name: ");
	(bind ?n (read))
	(printout t "Relation: ")
	(bind ?r (read))
	(assert (entity ?n ?r ))
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
	?f <- (state two)
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