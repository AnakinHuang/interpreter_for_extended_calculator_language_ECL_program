read int a
read int b
do 
    check a != b
    if a > b
        a := a - b
    fi
    if b > a
        b := b - a
    fi
    if a == b
        write a
    fi
od
