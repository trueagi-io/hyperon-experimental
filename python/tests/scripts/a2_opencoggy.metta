(EvaluationLink
  (PredicateNode "eats")
  (ListLink
     (ConceptNode "Sam")
     (ConceptNode "flies")))


!(match &self
   (EvaluationLink
     (PredicateNode "eats")
     (ListLink
       $x
       (ConceptNode "flies")))
   $x)
