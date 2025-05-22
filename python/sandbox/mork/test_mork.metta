!(import! &self mork)

!((mork-space my_space) status)
!((mork-space my_space) import_uri
    "https://raw.githubusercontent.com/Adam-Vandervorst/metta-examples/refs/heads/main/aunt-kg/toy.metta")
!((py-atom time.sleep) 0.3)

;!(match (mork-space my-space) (parent $x $y) ($x $y))
!((mork-space my_space) export (parent $x $y) ($x $y))
