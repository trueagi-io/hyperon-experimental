(@doc random-int
  (@desc "Returns random int number from range defined by two numbers (second and third argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Range start")
    (@param "Range end")))
  (@return "Random int number from defined range"))

(@doc random-float
  (@desc "Returns random float number from range defined by two numbers (second and third argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Range start")
    (@param "Range end")))
  (@return "Random float number from defined range"))

(@doc set-random-seed
  (@desc "Sets a new seed (second argument) for random number generator (first argument)")
  (@params (
    (@param "Random number generator instance")
    (@param "Seed")))
  (@return "Unit atom"))

(@doc new-random-generator
  (@desc "Creates new random number generator instance using seed as input (first argument)")
  (@params (
    (@param "Seed")))
  (@return "Instance of random number generator"))

(@doc reset-random-generator
  (@desc "Resets instance of random number generator (first argument) to its default behavior (StdRng::from_os_rng())")
  (@params (
    (@param "Random number generator instance")))
  (@return "Random number generator instance with default behavior"))

(@doc flip
  (@desc "Random boolean value")
  (@params ())
  (@return "Returns uniformly distributed random boolean value"))