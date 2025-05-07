read int n
read real found
read n
read found
int aa := 99
real bb := 9.
real cc := 9.99
real dd := 9.e9
real ee := .99
real ff := .99e99
real gg := .99e+9
real hh := .99e-9
int aaa := 9 * (8 * ( 6 + 7 ))
real bbb := (( 6. + 7. ) * 8.) * 9.
write aaa
write bbb
do
    check n > (( 5 + 8 ) * 7) * 9
od
do
    check 9 * (8 * ( 6 + 7 )) < n
    check n >= (( 5 + 8 ) * 7) * 9
    check 9 * (8 * ( 6 + 7 )) <= n
    check n <= trunc (9. * (8.8 * ( 6.e6 + .7e7 )))
    check float (9 * (8 * ( 6 + 7 ))) <= float (n)
od
if found == 9. / (8.8 * ( 6.e6 + .7e7 ))
fi
if found != (8.8 * ( 6.e6 + .7e7 )) / 9.
    if 9 * (8 * ( 6 + 7 )) < n fi
    if n > (( 5 + 8 ) * 7) * 9 fi
    if 9 * (8 * ( 6 + 7 )) <= n fi
    if n >= trunc (9. * (8.8 * ( 6.e6 + .7e7 ))) fi
    if float (9 * (8 * ( 6 + 7 ))) == float (n) fi
fi
