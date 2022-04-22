(EvaluationLink
  (PredicateNode "eats")
  (ListLink
     (ConceptNode "Sam")
     (ConceptNode "flies")))


!(println!
  (match &self
    (EvaluationLink
      (PredicateNode "eats")
      (ListLink
        $x
        (ConceptNode "flies")))
    $x))

