getYears :: Float -> Float
getYears seconds = seconds / 31557600.0

getMecuryAge :: Float -> Float
getMecuryAge years = years / 0.2408467

getVenusAge :: Float -> Float
getVenusAge years = years / 0.61519726

getMarsAge :: Float -> Float
getMarsAge years = years / 1.8808158

getJupiterAge :: Float -> Float
getJupiterAge years = years / 11.862615

getSaturnAge :: Float -> Float
getSaturnAge years = years / 29.447498

getUranusAge :: Float -> Float
getUranusAge years = years / 84.016846

getNeptuneAge :: Float -> Float
getNeptuneAge years = years / 164.79132

  
getSpaceAge age = do { let years = getYears age
; putStrLn (show years)
; putStrLn (show (getMecuryAge years))
; putStrLn (show (getVenusAge years))
; putStrLn (show (getMarsAge years))
; putStrLn (show (getJupiterAge years))
; putStrLn (show (getSaturnAge years))
; putStrLn (show (getUranusAge years))
; putStrLn (show (getNeptuneAge years))
} 
