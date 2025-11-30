param ([String]$ModNm)

# Update cabal file
$CabalPath = "AdventOfCode2025.cabal"

Write-Output ("Updating " + $CabalPath + "...")
$CabalContent = Get-Content -Path $CabalPath -Raw
$CabalContent = $CabalContent -replace ',.*([\r\n]+\s*)--\^\^Current\^\^',(',' + $ModNm + '$1--^^Current^^' )
Set-Content -Path $CabalPath $CabalContent

# Update main file
$MainPath = "app/Main.hs"
Write-Output ("Updating " + $MainPath + "...")
$MainContent = Get-Content -Path $MainPath -Raw
$MainContent = $MainContent -replace '(\s)\w+(\.[\w\.]+)? -- <<Current<< ',('$1' + $ModNm + '$2 -- <<Current<< ' )
Set-Content -Path $MainPath $MainContent
