(deffacts states
	(state initial); intiial, will, trust, revision?
	(category NULL);
	(moreFamily NULL); assume they have more family until not
	(moreAssets yes);
	(moreDebts yes);
)


; INITIAL STATE RULE
(defrule rule1
	?f <- (state initial)
	=>
	(printout t "Do you want to make a will or a trust?: ")
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
	(printout t "List all your relational entities, family, friends, organizations in this fassion (\"entity\\nrelation\" type help for a list of relations. When done type \"no\\nmore\": " crlf "-------" crlf "Name: ")
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
	?f <- (entity no more)
	=>
	(retract ?g)
	(retract ?f)
	(assert (moreFamily no))
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
