read real d
real l := d / 2.0
do
    check l * l > d
    l := l / 2.0
od
real h := 2.0 * l
real err := d - (l * l)
if err < 0.0 err := 0.0 - err fi
do
    check err > 1.e-10
    real a := (l + h) / 2.0
    if (a * a) < d l := a fi
    if (a * a) > d h := a fi
    err := d - (l * l)
    if err < 0.0 err := 0.0 - err fi
od
write l
