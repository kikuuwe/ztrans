(* ::Package:: *)

(*********************************************************)
rZTransFunc[fnc_]:=Module[{rZConv,rNume,rDeno,rNumeCoef,rDenoCoef,rB,rA},
	rZConv[iz_]:=fnc[2/T*(1-iz)/(1+iz)]//Together;
	rNume[iz_]:=Numerator[rZConv[iz]];
	rDeno[iz_]:=Denominator[rZConv[iz]];
	rNumeCoef = CoefficientList[rNume[iz],iz];
	rDenoCoef = CoefficientList[rDeno[iz],iz];
	rB = rNumeCoef/rDenoCoef[[1]] //Print;
	rA = rDenoCoef/rDenoCoef[[1]]//Print;
]
rTransFunc[s_]:=1/(M s^2 +  B s + K);
rZTransFunc[rTransFunc]
rTransFunc[s_]:=1/(B s + K);
rZTransFunc[rTransFunc]
