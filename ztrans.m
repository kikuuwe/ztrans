(* ::Package:: *)

(*******************************************************************************
  ztrans
  
  ztrans is a Mathematica notebook to derive a z-transform transfer function from
  a Laplace-transform transfer function.
  
  Author: Ryo Kikuuwe
  
  Copyright (c) 2014-2015 Ryo Kikuuwe
  
  The "sgn-sat" is a free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.
  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.
  
  You should have received a copy of the GNU General Public License
  along with this program; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA
  
  Contact: Ryo Kikuuwe, kikuuwe@ieee.org
*******************************************************************************)


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
