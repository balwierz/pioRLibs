screenCalc = function(diagInch, ratio9)
{
	return(c(widthCm=diagInch * 2.54 * sin(atan(ratio9/9)), heightCm=diagInch * 2.54 * cos(atan(ratio9/9))))
}
