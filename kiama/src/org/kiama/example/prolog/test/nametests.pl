anatomnotused.

anatomleft.
apredleftprev (1) :- anatomleft, anatomleft (1).
apredleftprev (1) :- apredleftprev (1), apredleftprev (1,2).

apredlefthere (1,2,3) :- apredlefthere (1,20), apredlefthere(3,4,5).

aprednotused (0,1) :- apredright (1,2), anatomright.
apredright (1) :- apredright (1,2,3).
apredright (1,2) :- apredright (1,2).
anatomright :- anatomright (1).
anatomright (1) :- anatomright.
