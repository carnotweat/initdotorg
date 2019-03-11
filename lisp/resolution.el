
;;;
;;; Resolution Refutation Theorem Prover
;;; by Stuart C. Shapiro
;;; April 2, 1993
;;; modified April 11, 1996
;;;
;;; This is a pedagogical resolution theorem prover.
;;; There are multiple ways to improve it.
;;;
;;; The external functions are:
;;;
;;;     (prover:prove <list of assumption wffs> <wff to be proven>)
;;;        Tries to show that <wff to be proven> follows from
;;;           <list of assumption wffs>
;;;           using resolution refutation
;;;           with the set-of-support and unit-preference strategies,
;;;           and removing tautologies when they appear.
;;;
;;;     (prover:unify <atomic wff> <atomic wff>)
;;;        Returns the mgu of the two wffs, or FAIL.
;;;
;;;     (prover:clauseForm <wff>)
;;;        Returns a list of clauses equivalent to <wff>.
;;;
;;; The parameter prover:*UseAnswer* determines whether or not the
;;; ANSWER predicate will be used.
;;;
;;; The parameter prover:*Guided* determines if the program will pick
;;; the clauses to be resolved, or if the user will.
;;;
;;; The syntax of wffs accepted by these functions are:
;;;
;;; <logical constant> ::= ~ | & | or | => | <=> | all | exists
;;;
;;; <symbol> ::= {any Lisp symbol that doesn't start with the character #\?
;;;               except a <logical constant>} 
;;;
;;; <individual constant> ::= <symbol>
;;; <variable> ::= <symbol>
;;; <function> ::= <symbol>
;;; <predicate> ::= <symbol>
;;; <propositional constant> ::= <symbol>
;;;      {Note: Although <individual constant>s, <variable>s, <function>s,
;;;       <predicate>s, and <propositional constant>s are all <symbol>s, they
;;;       should be different from each other.  That is, no <symbol> should be
;;;       used as a member of more than one of these five categories.}
;;;
;;; <functional term> ::= ( <function> <term>* )
;;;      {Note: It is standard practice that every use of a particular
;;;       <function> have the same number of arguments.}
;;;
;;; <term> ::= <individual constant> | <variable> | <functional term>
;;;
;;; <atomic wff> ::= <propositional constant> | ( <predicate> <term>* )
;;;      {Note: It is standard practice that every use of a particular
;;;       <predicate> have the same number of arguments.}
;;;
;;; <literal> ::= <atomic wff> | (~ <atomic wff>)
;;;
;;; <clause> ::= ( <literal>* )
;;;
;;; <wff> ::= <atomic wff> |
;;;           (~ <wff>) |
;;;           (<wff> or <wff>) |
;;;           (<wff> & <wff>) |
;;;           (<wff> => <wff>) |
;;;           (<wff> <=> <wff>) |
;;;           (all <variable> <wff>) |
;;;           (exists <variable> <wff>)
;;;
;;; Note that there is no special syntax for variables in <wff>s---they are
;;; recognized by their position in a quantified <wff>.  However, in clauses
;;; constructed by prover:clauseForm, variables start with the character #\?.
;;; That is why an arbitrary symbol cannot start with that character.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage "PROVER"
    (:shadow common-lisp:pi)
  (:export prove unify clauseForm qed *UseAnswer* *Guided*))

(in-package "PROVER")

(defparameter *UseAnswer* nil
  "If T, the ANSWER predicate will be used;
   If NIL, it won't")

(defparameter *Guided* nil
  "If T, the user will be asked for the clauses to be resolved;
   Else, the program will choose them.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of the Proof Line structure

(defstruct (line
	     (:print-function
	       (lambda (line stream depth)
		 (declare (ignore depth))
		 (format stream "~2D  ~A~20,2T~A"
			 (line-number line)
			 (line-clause line)
			 (line-justification line)))))
  "A Line of the proof."
  number ; The line number.
  clause ; The actual clause.
  justification ; Either "Assumption," "From Query"
		; or a pair of line numbers.
  )

(defvar *previous-line-number* 0
  "A global counter for line numbers.")

(defun genLineNo ()
  "Increments *previous-line-number* and returns it."
  (setf *previous-line-number* (1+ *previous-line-number*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for Resolution Refutation Theorem Proving

(defun prove (assumptions theorem)
  "Attempts to prove that the wff THEOREM
   follows from the list of wffs ASSUMPTIONS
   using Resolution refutation
   with the unit-preference and set-of-support strategies."
  (setf common-lisp:*gensym-counter* 0	; To produce new variables
				; and Skolem functions with small integers.
	*previous-line-number* 0)		       
  (catch 'prove
    (refute (mapcar #'(lambda (cl)
			(make-line :number (genLineNo)
				   :clause cl
				   :justification "Assumption"))
		    (sortByLength (mapcan #'clauseform assumptions)))
	    (mapcar #'(lambda (cl)
			(make-line :number (genLineNo)
				   :clause cl
				   :justification "From Query"))
		    (sortByLength (clauseform
				   (if *UseAnswer* (AddAnswer theorem)
				     `(~ ,theorem))))))))

(defun refute (oldLines newLines)
  "Tries to derive the null clause from the clauses in
   OLDLINES and NEWLINES.
   Uses the unit-preference and set-of-support strategies,
   and assumes that the clauses in NEWLINES are supported,
   and that the two lists of Lines are ordered by clause length,
   shortest first."
  (mapc #'print oldLines)
  (setf oldLines (remove-if #'remove-tautologyp oldLines))
  (mapc #'print newLines)
  (setf newLines (remove-if #'remove-tautologyp newLines))
  (dolist (line1 oldLines)
    (setf oldLines 
      (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		 oldLines)
      newLines 
      (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		 newLines)))
  (dolist (line1 newLines)
    (setf oldLines 
      (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		 oldLines)
      newLines 
      (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		 newLines)))
  (loop
    (unless newLines (return nil))
    (let* ((line (if *Guided* (get-a-line NewLines OldLines)
		   (first newLines)))
	      (resolvants 
	       (resolvants line 
			   (if *Guided* (get-another-line NewLines OldLines)
			     oldLines))))
      (dolist (line1 oldLines)
	(setf resolvants 
	  (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		     resolvants)))
      (setf newLines (mergeLineLists resolvants (rest newLines)))
      (dolist (line1 resolvants)
	(setf oldLines 
	  (remove-if #'(lambda (line2) (remove-subsumed line1 line2))
		     oldLines)))
      (setf oldLines (mergeLineLists (list line) oldLines)))))

(defun get-a-line (NewLines OldLines)
  "Get the number of a clause to be resolved from the user,
   and return the Line with that number."
  (format t "~&Enter the number of a clause that you want to resolve with another:  ")
  (loop
    (let* ((number (read))
	   (line
	    (or (find number NewLines :key #'line-number :test #'=)
		(find number OldLines :key #'line-number :test #'=))))
      (when line (return-from get-a-line line)))
    (format t "~&I couldn't find that one, please try again: ")))

(defun get-another-line (NewLines OldLines)
  "Get the number of a clause to be resolved from the user,
   and return the Line with that number."
  (format t
	  "~&Enter the number of a clause that you want to resolve that one with:  ")
  (loop
    (let* ((number (read))
	   (line
	    (or (find number NewLines :key #'line-number :test #'=)
		(find number OldLines :key #'line-number :test #'=))))
      (when line (return-from get-another-line (list line))))
    (format t "~&I couldn't find that one, please try again: ")))

(defun resolvants (line lines)
  "Returns an ordered list of all resolvants obtainable by resolving LINE's
   clause with each clause in LINES."
  (when lines
    (mergeLineLists 
     (resolve (line-clause line)
	      (line-clause (first lines))
	      (line-number line)
	      (line-number (first lines)))
     (resolvants line (rest lines)))))

(defun resolve (cl1 cl2 num1 num2)
  "Returns an ordered list of the resolvants of clause CL1 with clause CL2,
   and notes that these descend from the Lines whose numbers are NUM1 and
   NUM2." 
  (remove-if
   #'remove-tautologyp
   (let (resolvants)
     (dolist (lit1 cl1 resolvants)
       (dolist (lit2 cl2)
	 (multiple-value-bind (at1 at2)
	     (getAtoms lit1 lit2)
	   (when at1
	     (let ((mgu (unify at1 at2)))
	       (unless (eql mgu 'FAIL)
		 (when (nullClausep
			(first
			 (setf resolvants
			   (mergeLineLists
			    (list
			     (print
			      (make-line
			       :number (genLineNo)
			       :clause (renameClause
					(applySub (union (remove lit1 cl1)
							 (remove lit2
								 cl2)
							 :test #'equal)
						  mgu))
			       :justification
			       (format nil "R ~2D ~2D ~A" num1 num2 mgu))))
			    resolvants))))
		   (throw 'prove 'QED)))))))))))

(defun getAtoms (lit1 lit2)
  "If one of LIT1 and LIT2 is a negative literal,
   and the other is a positive literal,
   returns the two atomic formulae;
   else returns NIL and NIL."
  (cond ((negWffp lit1)
	 (if (not (negWffp lit2))
	     (values (second lit1) lit2)
	     (values nil nil)))
	((negWffp lit2)
	 (values lit1 (second lit2)))
	(t (values nil nil))))

(defun shorter (x y)
  "Returns T if x is a shorter clause than y;
   NIL if it's a longer list."
  (check-type x list)
  (check-type y list)
  (< (clauseLength x) (clauseLength y)))

(defun sortByLength (list)
  "Destructively sorts the elements of LIST by their length,
   shortest first."
  (check-type list list)
  (sort list #'shorter))

(defun mergeLineLists (list1 list2)
  "Returns a list consisting of all Lines in LIST1 and LIST2.
   LIST1 and LIST2 are assumed to be ordered by clause length,
   and so will be the resulting list."
  (cond ((null list1) list2)
	((null list2) list1)
	(t (let* ((line1 (first list1))
		  (line2 (first list2))
		  (c1 (line-clause line1))
		  (c2 (line-clause line2))
		  (l1 (clauseLength c1))
		  (l2 (clauseLength c2)))
	     (cond ((= l1 l2)
		    (cond ((subsumes c2 c1)
			   (removing-subsumed line1 line2)
			   (mergeLineLists (rest list1) list2))
			  ((subsumes c1 c2)
			   (removing-subsumed line2 line1)
			   (mergeLineLists list1 (rest list2)))
			  (t (cons line2
				   (remove-if
				    #'(lambda (line)
					(remove-subsumed line2 line))
				    (mergeLineLists list1 (rest list2)))))))
		   ((< l1 l2)
		    (cond ((subsumes c1 c2)
			   (removing-subsumed line2 line1)
			   (mergeLineLists list1 (rest list2)))
			  (t (cons line1
				   (remove-if
				    #'(lambda (line)
					(remove-subsumed line1 line))
				    (mergeLineLists (rest list1) list2))))))
		   (t (cond ((subsumes c2 c1)
			     (removing-subsumed line1 line2)
			     (mergeLineLists (rest list1) list2))
			    (t (cons line2
				     (remove-if
				      #'(lambda (line)
					  (remove-subsumed line2 line))
				      (mergeLineLists list1 (rest list2))))))
		      ))))))

(defun nullClausep (line)
  "Returns T if LINE's clause is the empty clause
   or the clause all of whose literals are ANSWER literals;
   else NIL."
  (check-type line line)
  (labels ((answerclausep (clause)
	     (or (null clause)
		 (and (listp (first clause))
		      (eql (first (first clause)) 'ANSWER)
		      (answerclausep (rest clause))))))
    (answerclausep (line-clause line))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of Logical Constants and Kinds of WFFs

(defconstant *bicond* '<=>
  "The biconditional operator.")

(defconstant *bicondName* (symbol-name *bicond*)
  "The name of the biconditional operator.")

(defun bicondWffp (wff)
  "Returns T if the main connective of WFF is iff; else NIL."
  (and (listp wff)
       (opNamed (second wff) *bicondName*)))

(defconstant *cond* '=>
  "The conditional operator.")

(defconstant *condName* (symbol-name *cond*)
  "The name of the conditional operator.")

(defun condWffp (wff)
  "Returns T if the main connective of WFF is implication; else NIL."
  (and (listp wff)
       (opNamed (second wff) *condName*)))

(defconstant *and* '&
  "The conjunction operator.")

(defconstant *andName* (symbol-name *and*)
  "The name of the conjunction operator.")

(defun andWffp (wff)
  "Returns T if the main connective of WFF is conjunction; else NIL."
  (and (listp wff)
       (opNamed (second wff) *andName*)))

(defconstant *or* 'or
  "The disjunction operator.")

(defconstant *orName* (symbol-name *or*)
  "The name of the disjunction operator.")

(defun orWffp (wff)
  "Returns T if the main connective of WFF is disjunction; else NIL."
  (and (listp wff)
       (opNamed (second wff) *orName*)))

(defconstant *not* '~
  "The negation operator.")

(defconstant *notName* (symbol-name *not*)
  "The name of the negation operator.")

(defun negWffp (wff)
  "Returns T if the main connective of WFF is negation; else NIL."
  (and (listp wff)
       (opNamed (first wff) *notName*)))

(defconstant *uQuant* 'all
  "The universal quantifier.")

(defconstant *uQuantName* (symbol-name *uQuant*)
  "The name of the universal quantifier.")

(defun uQuantWffp (wff)
  "Returns T if WFF is universally quantified; else NIL."
  (and (listp wff)
       (opNamed (first wff) *uQuantName*)))

(defconstant *eQuant* 'exists
  "The existential quantifier.")

(defconstant *eQuantName* (symbol-name *eQuant*)
  "The name of the existential quantifier.")

(defun eQuantWffp (wff)
  "Returns T if WFF is existentially quantified; else NIL."
  (and (listp wff)
       (opNamed (first wff) *eQuantName*)))

(defun opNamed (o opname)
  "Returns T if O is the same as the operator whose name is OPNAME;
   NIL otherwise."
  (and (symbolp o) (string= (symbol-name o) opname)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of New Variables

(defmacro newVar ()
  "Returns a newly created variable."
  `(gensym "?"))

(defun variablep (o)
  "Returns T if O is a variable created by newVar; NIL otherwise."
  (and (symbolp o) (char= (char (symbol-name o) 0) #\?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for Clause Form

(defun clauseForm (wff)
  (mapcar #'renameClause
	  (clauses
	    (CNF
	     (Skolemize
	      (miniScope (removeCond (removeBicond (renameWff wff)))))))))

(defun renameWff (wff)
  "Returns a wff equavalent to WFF,
   but with every quantifier binding a unique variable."
  (renameWff1 wff '()))

(defun renameWff1 (wff sub)
  "Returns a wff like WFF,
   but with every quantifier binding a unique variable,
   and with every free variable changed to what it is bound to in SUB."
   (cond ((atom wff) (applySeqSubToAtom wff sub))
	 ((or (uQuantWffp wff)
	      (eQuantWffp wff))
	  (let ((newVar (newVar)))
	    (list (first wff)
		  newVar
		  (renameWff1
		   (third wff)
		   (conspair newVar (second wff) sub)))))
	 (t (mapcar #'(lambda (x) (renameWff1 x sub))
		    wff))))

(defun removeBicond (wff)
  "Changes every subformula of WFF of the form (P <=> Q)
   into ((P => Q) & (Q => P))"
  (cond ((atom wff) wff)
	((bicondWffp wff)
	 (let ((p (removeBicond (first wff)))
	       (q (removeBicond (third wff))))
	   (list (list p *cond* q) *and* (list q *cond* p))))
	(t (mapcar #'removeBicond wff))))

(defun removeCond (wff)
  "Changes every subformula of WFF of the form (P => Q)
   into ((~ P) or Q).
   WFF is presumed to have no biconditionals."
  (cond ((atom wff) wff)
	((condWffp wff)
	 (list (list *not* (removeCond (first wff)))
	       *or*
	       (removeCond (third wff))))
	(t (mapcar #'removeCond wff))))

(defun miniScope (wff)
  "Returns a f wff equivalent to WFF,
   but with all negation operators in minimum scope..
   WFF is presumed to have no biconditionals or conditionals."
  (cond ((atom wff) wff)
	((negWffp wff)
	 (moveInNot (second wff)))
	(t (mapcar #'miniScope wff))))

(defun moveInNot (wff)
  "Returns a wff equivalent to (~ WFF),
   but with all negation operators in minimum scope."
  (cond  ((atom wff) (list *not* wff))
	 ((uQuantWffp wff)
	  (list *eQuant*
		(second wff)
		(moveInNot (third wff))))
	 ((eQuantWffp wff)
	  (list *uQuant*
		(second wff)
		(moveInNot (third wff))))
	 ((negWffp wff)
	  (miniScope (second wff)))
	 ((orWffp wff)
	  (list (moveInNot (first wff))
		*and*
		(moveInNot (third wff))))
	 ((andWffp wff)
	  (list (moveInNot (first wff))
		*or*
		(moveInNot (third wff))))
	 (t (list *not* wff))))

(defun Skolemize (wff)
  "Returns a wff like WFF,
   but with every existentially quantified variable replaced
   by a Skolem function of all universally quantified variables
   in whose scope the existentially quantified variable is,
   and with all the quantifiers deleted."
  (Skolemize1 wff '() '()))

(defun Skolemize1 (wff uvars sub)
  "Returns a Skolemized version of WFF,
   which is in the scope of the universal quantifiers that bind
   all the variables in UVARS,
   and, in addition, every variable in the sequential substitution, SUB,
   is replaced by its term in SUB."
  (cond ((atom wff) (applySeqSubToAtom wff sub))
	((uQuantWffp wff)
	 (Skolemize1 (third wff) (cons (second wff) uvars) sub))
	((eQuantWffp wff)
	 (Skolemize1 (third wff)
		     uvars
		     (conspair
		       (if uvars (cons (gensym "S") uvars)
			   (gensym "S"))
		       (second wff) sub)))
	(t (mapcar #'(lambda (x) (Skolemize1 x uvars sub)) wff))))

(defun CNF (wff)
  "Returns a wff equivalent to WFF,
   but in Conjunctive Normal Form.
   The only connectives in WFF are AND, OR, and NOT,
   and NOTs are assumed to be in miniscope."
  (cond ((andWffp wff)
	 (list (CNF (first wff)) *and* (CNF (third wff))))
	((orWffp wff)
	 (distrib (list (CNF (first wff)) *or* (CNF (third wff)))))
	(t wff)))

(defun distrib (wff)
  "Applies the distributive law
   to turn wff, whose main connective is OR, and whose two operands are in CNF,
   into CNF."
  (cond ((andWffp (first wff))
	 (list (CNF (list (first (first wff)) *or* (third wff)))
	       *and*
	       (CNF (list (third (first wff)) *or* (third wff)))))
	((andWffp (third wff))
	 (list (CNF (list (first wff) *or* (first (third wff))))
	       *and*
	       (CNF (list (first wff) *or* (third (third wff))))))
	(t wff)))

(defun clauses (wff)
  "Returns a list of clauses corresponding to the CNF formula WFF."
  (cond ((andWffp wff)
	 (append (clauses (first wff))
		 (clauses (third wff))))
	((orWffp wff)
	 (list (append (literals (first wff))
		       (literals (third wff)))))
	(t (list (list wff)))))

(defun literals (wff)
  "Returns a list of all the literals in WFF.
   The only connectives in WFF are OR and NOT,
   and NOT is in miniscope."
  (if (orWffp wff)
      (append (literals (first wff)) (literals (third wff)))
      (list wff)))

(defun renameClause (cl)
  "Returns a clause like CL,
   but with every variable replaced by a fresh one."
  (renameClause1 cl '()))

(defun renameClause1 (cl sub)
  "Returns a clause like CL,
   but with every variable in the sequential substitution SUB
       replaced by its term in SUB,
   and with every other variable replaced by a fresh one.
   Also returns as a second value the final SUB."
  (cond ((variablep cl)
	 (if (boundIn cl sub)
	     (values (applySeqSubToAtom cl sub) sub)
	     (let ((newVar (newVar)))
	       (values newVar (consPair newVar cl sub)))))
	((atom cl) (values cl sub))
	(t
	 (multiple-value-bind (firstcl newsub)
	     (renameClause1 (first cl) sub)
	   (multiple-value-bind (restcl lastsub)
	       (renameClause1 (rest cl) newsub)
	     (values (cons firstcl restcl) lastsub))))))

(defun addAnswer (wff)
  "Adds the ANSWER predicate to WFF---the theorem to be proved."
  ;; Assumes that wff is of the form
  ;;    (exists x (exists y ... (exists z wff) ...))
  ;;    where wff has no quantifiers
  (cond ((endp wff) 'ANSWER)
	((eQuantWffp wff)
	 (list *uQuant*
	       (second wff)
	       (addAnswer (third wff))))
	(t (list wff *cond* (list 'ANSWER wff)))))

(defun clauseLength (clause)
  "Returns the number of literals in CLAUSE,
   ignoring ANSWER predicates."
  (cond ((endp clause) 0)
	((atom (first clause)) (1+ (clauseLength (rest clause))))
	((eql (first (first clause)) 'ANSWER)
	 (clauseLength (rest clause)))
	(t (1+ (clauseLength (rest clause))))))

(defun tautologyp (clause)
  "Returns T if CLAUSE is a tautology; NIL otherwise."
  (some #'(lambda (literal)
	    (and (negWffp literal)
		 (member (second literal) clause :test #'equal)))
	clause))

(defun remove-tautologyp (line)
  "If the clause of LINE is a tautology,
   prints a message that the LINE is being deleted,
   and returns T;
   else returns NIL."
  (when (tautologyp (line-clause line))
    (format t "~&Deleting tautology: ~2D  ~A"
	    (line-number line)
	    (line-clause line))
    t))

(defun subsumes (c1 c2)
  "Returns T if clause C1 subsumes clause C2; NIL otherwise."
  ;; C1 subsumes C2 if there is a substitution s,
  ;; such that C1s is a subset of C2.
  ;; If C1 subsumes C2, C2 can be removed from the set of clauses.
  (and (not (eq c1 c2))
       (or (subsetp c1 c2 :test #'equal)
	   (not (eql (instance-subset
		      (remove-if #'(lambda (literal)
				     (member literal c2 :test #'equal))
				 c1)
		      c2)
		     'FAIL)))))

(defun instance-subset (c1 c2 &optional subst)
  "If some instance of clause C1SUBST is a subset of clause C2,
   returns the substitution sigma s.t. C1sigma is a subset of C2;
   Else returns NIL."
  ;; We know that C1 is not a subset of C2 without a substitution
  (cond ((eql subst 'FAIL) 'FAIL)
	((null c1) subst)
	(t (let ((lit1 (first c1))
		 newsub)
	     (dolist (lit2 c2 'FAIL)
	       (setf newsub (match-literals lit1 lit2 subst))
	       (unless (eql newsub 'FAIL)
		 (setf newsub (instance-subset (rest c1) c2 newsub))
		 (unless (eql newsub 'FAIL) (return newsub))))))))

(defun match-literals (l1 l2 &optional subst)
  "If some substitution instance of L1 matches L2, returns the
   substitution, else returns FAIL."
  (labels ((rml (l1 l2 subst)
	     (cond ((equal l1 l2) subst)
		   ((variablep l1)
		    (cond ((boundIn l1 subst)
		           (if (equal (bindingOf l1 subst) l2) subst
                               (throw 'rml-tag 'FAIL)))
	                  ((eql (catch 'unify (MakeSureNotInR l1 l2)) 'FAIL)
		           (throw 'rml-tag 'FAIL))
	                  (t (bind l1 l2 subst))))
		   ((or (atom l1) (atom l2)) (throw 'rml-tag 'FAIL))
		   (t (rml (rest l1) (rest l2)
			   (rml (first l1) (first l2) subst))))))
    (catch 'rml-tag (rml l1 l2 subst))))

(defun remove-subsumed (line1 line2)
  "If the clause of LINE2 is subsumed by the clause of LINE1,
   prints a message that LINE2 is being deleted,
   and returns T;
   else returns NIL."
  (when (subsumes (line-clause line1) (line-clause line2))
    (removing-subsumed line2 line1)
    t))

(defun removing-subsumed (subsumedLine subsumingLine)
  "Prints a message that subsumedLine is being deleted."
  (format t "~&Deleting ~2D ~A~%because it's subsumed by ~2D ~A"
	  (line-number subsumedLine) (line-clause subsumedLine)
	  (line-number subsumingLine) (line-clause subsumingLine)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for Sequential Substitutions
;;;
;;; These are like regular substitutions, but are applied sequentially
;;; beginning with the first pair, instead of in parallel.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun conspair (term var sub)
  "Returns a substitution like SUB,
   but with the pair TERM/VAR cons'd on front."
  (check-type var symbol)
  (cons (cons var term) sub))

(defun boundIn (o sub)
  "Returns T if O is an atom of SUB; NIL otherwise."
  (assoc o sub))

(defun applySeqSubToAtom (atom sub)
  "If ATOM is bound to some term in SUB,
   returns that term;
   else returns ATOM."
  (or (cdr (assoc atom sub)) atom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Definition of Unification

(defun unify (s1 s2)
  "If s1 and s2 are unifiable S-expressions,
      returns their most general unifier;
   Otherwise, returns FAIL."
  (catch 'unify (unify1 s1 s2 nil)))

(defun unify1 (s1 s2 b)
  "If s1b and s2b are unifiable (where sib means the result of 
      applying the substitution b to the S-expression si)
      with most general unifier c, returns bc.
      I.e., in that case, returns the most general unifier of s1 and s2.
   Otherwise, raises a (throw 'unify 'fail) exception condition.
   Assumes that no variable of b occurs in a term of b."
       (cond ((eql s1 s2) b)
	     ((variablep s1)
	      (cond ((bboundp s1 b) (unify1 (bindingOf s1 b) s2 b))
		    (t (bind s1 (occursCheckingApply s2 b s1) b))))
	     ((variablep s2)
	      (cond ((bboundp s2 b) (unify1 s1 (bindingOf s2 b) b))
		    (t (bind s2 (occursCheckingApply s1 b s2) b))))
	     ((or (atom s1) (atom s2)) (throw 'unify 'fail))
	     ((/= (length s1) (length s2)) (throw 'unify 'fail))
	     (t (unify1 (rest s1)
			(rest s2)
			(unify1 (first s1) (first s2) b)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for regular Substitutions

(defun bboundp (v b)
  "Returns T if the variable v is a variable of the substitution b.
           (I.e. if v is bound in b.)
   Otherwise, returns NIL."
  (assoc v b))

(defun bindingOf (v b)
  "Returns the term paired with the variable v in the substitution b.
  In other words, assuming v is a variable, returns vb.
  In still other words, returns the binding of the variable v in b."
  (cdr (assoc v b)))

(defun bind (v s b)
  "Returns b{s/v}, the composition of the substitution b with 
        the substitution {s/v}.
   If s = v, just returns b unchanged.
   As long as v does not occur in s, v will not occur in any term
        of b{s/v}.
   Assumes that v is not already a variable of b."
  (if (eql s v) b
      (cons (cons v s) (substituteInTerms s v b))))

(defun substituteInTerms (s v b)
  "Returns a copy of the substitution b
       with every term t replaced by t{s/v}."
  (mapcar #'(lambda (pair)
	      (cons (first pair) (subst s v (rest pair))))
	  b))

(defun applySub (wff sub)
  "Returns a copy of WFF,
   but with every variable in sub
   replaced by its term in sub."
  (cond ((atom wff) (or (bindingOf wff sub) wff))
	(t (mapcar #'(lambda (argi) (applySub argi sub))
		   wff))))

;;; The next two functions, occursCheckingApply and MakeSureNotIn, each come in
;;; two flavors: a function for the top level; and a function for all deeper
;;; levels.  This is because, for example, x does not occur in x, but it does
;;; in (f x) or (f (f x)) or (f (f ... x)).

(defun occursCheckingApply (s b v)
  "Returns sb (where s is an S-expression, and b is a substitution).
   However, if sb is non-atomic and the variable v occurs in sb, 
        raises the (throw 'unify 'fail) exception condition.
   Assumes v is not a variable of b, 
           and that no variable of b is in a term of b."
  (cond ((and (variablep s) (bboundp s b))
	 (makeSureNotIn v (bindingOf s b)))
	((atom s) s)
	(t (mapcar #'(lambda (si) (occursCheckingApplyR si b v))
		   s))))

(defun occursCheckingApplyR (s b v)
  "Returns sb (where s is an S-expression, and b is a substitution).
   However, if the variable v occurs in sb, 
        raises the (throw 'unify 'fail) exception condition.
   Assumes v is not a variable of b, 
           and that no variable of b is in a term of b."
  (cond ((and (variablep s) (bboundp s b))
	 (makeSureNotInR v (bindingOf s b)))
	((eql s v) (throw 'unify 'fail))
	((atom s) s)
	(t (mapcar #'(lambda (si) (occursCheckingApplyR si b v))
		   s))))

(defun makeSureNotIn (a s)
  "If s is non-atomic, and the atom a is anywhere in the S-expression s,
        raises the (throw 'unify 'fail) exception condition.
   Otherwise, returns s."
  (cond ((atom s) s)
	(t (mapcar #'(lambda (si) (makeSureNotInR a si))
		   s))))

(defun makeSureNotInR (a s)
  "If the atom a is anywhere in the S-expression s,
        raises the (throw 'unify 'fail) exception condition.
   Otherwise, returns s."
  (cond ((eql a s) (throw 'unify 'fail))
	((atom s) s)
	(t (mapcar #'(lambda (si) (makeSureNotInR a si))
		   s))))
