/*
a:int!0 ++ c:int?x.o1:int!0 || Tau.a:int?x.Disc() ++ c:int!4 || c:int!10
1. a:int!0 ++ c:int?x.o1:int!0 || a:int?x.Disc() || c:int!10
2. o1:int!0 || c:int!4

a:int!0 || Tau.a:int?x.Disc() ++ c:int!4 
a:int!0 || Tau.a:int?x.Disc()
a:int!0 || c:int!4 

c:int?x.o1:int!0 || o1:int!4 ++ c:int!4
c:int?x.o1:int!0 || c:int!4

a:int!0 ++ c:int?x.o1:int!0 || a:int?x.Disc() ++ c:int!4 || c:int!10
1. a:int!0 || a:int?x.Disc() || c:int!10
2. a:int!0 || c:int!4 || c:int!10
3. c:int?x.o1:int!0 || Tau.a:int?x.Disc() || c:int!10
4. c:int?x.o1:int!0 || c:int!4 || c:int!4
4bis. o1:int!0 || Tau.a:int?x.Disc() ++ c:int!4

a:int!0 ++ z:int!20 || a:int?x.Disc() ++ z:int!10 || z:int?p.Disc()
1. a:int!0 || a:int?x.Disc() || z:int?p.Disc()
2. a:int!0 || z:int!10 || z:int?p.Disc()
3. z:int!20 || a:int?x.Disc() || z:int?p.Disc()
4. z:int!20 || z:int!10 || z:int?p.Disc()

a:int!0 ++ z:int!20 || z:int!10 || z:int?p.Disc()

c:int!5 || c:int?x.o:int!x


before preprocessing: 
(sendA ++ sendB) || recvB.cont1 || (recvA.cont2 ++ Tau.discard)
valuto la scelta nondet più a sinistra
1. [match send/recv A] recvB.cont1 || cont2 -> niente da fare
2. [match send/recv B] cont1 || (recvA.cont2 ++ Tau.discard) -> valuto la scelta nondet più a sinistra
2.1 [c'è Tau] cont1 || Tau.discard

distributions after preprocessing: 
1. recvB.cont1 || cont2 
2. cont1 || Tau.discard

before preprocessing: 
(sendA ++ sendB) || recvB.cont1 || (recvA.cont2 ++ disc)
valuto la scelta nondet più a sinistra
1. [match send/recv A] recvB.cont1 || cont2 -> niente da fare
2. [match send/recv B] cont1 || (recvA.cont2 ++ discard) -> valuto la scelta nondet più a sinistra ma  niente da fare

distributions after preprocessing: 
1. recvB.cont1 || cont2
2. cont1 || (recvA.cont2 ++ discard)

before preprocessing: 
(sendA ++ sendB) || (recvB.cont1 ++ recvA.cont2)
valuto la scelta nondet più a sinistra
1. [match send/recv A] cont2 -> niente da fare
2. [match send/recv B] cont1 -> niente da fare

distributions after preprocessing: 
1. cont2
2. cont1

before preprocessing: 
(sendA ++ recvB.cont1) || (sendB ++ recvA.cont2)
valuto la scelta nondet più a sinistra
1. [match send/recv A] cont2 -> niente da fare
2. [match send/recv B] cont1 -> niente da fare

distributions after preprocessing: 
1. cont2
2. cont1

before preprocessing: 
(sendA ++ recvB.cont1) || (Tau.sendC ++ Tau.recvA.cont2)
valuto la scelta nondet più a sinistra
1. [match send/recv A] (sendA ++ recvB.cont1) || Tau.sendC -> niente da fare
2. [match send/recv B] (sendA ++ recvB.cont1) || Tau.recvA.cont2 -> -> niente da fare

distributions after preprocessing: 
1. (sendA ++ recvB.cont1) || Tau.sendC
2. (sendA ++ recvB.cont1) || Tau.recvA.cont2

before preprocessing: 
(sendA ++ recvX.cont1) || (recvX.sendC ++ recvY.recvA.cont2) || sendX || sendY
valuto la scelta nondet più a sinistra
1. [match send/recv A] (sendA ++ recvB.cont1) || sendC || sendY -> chiamata ricorsiva
2. [match send/recv B] (sendA ++ recvB.cont1) || Tau.recvA.cont2 -> -> niente da fare

distributions after preprocessing: 
1. (sendA ++ recvB.cont1) || Tau.sendC
2. (sendA ++ recvB.cont1) || Tau.recvA.cont2

before preprocessing: 
(sendA ++ recvB.cont1) || (sendB ++ recvA.cont2) || (altro1 ++ altro2)
valuto la scelta nondet più a sinistra
1. [match send/recv A] cont2 || (altro1 ++ altro2) -> niente da fare
2. [match send/recv B] cont1 || (altro1 ++ altro2) -> niente da fare

distributions after preprocessing: 
1. cont2 || (altro1 ++ altro2)
2. cont1 || (altro1 ++ altro2)

(sendX ++ recvX.cont1) || (recvX.sendC ++ recvY.recvA.cont2) || sendX || sendY
1. sendC || sendX || sendY
2. cont1 || (recvX.sendC ++ recvY.recvA.cont2) || sendY
*/