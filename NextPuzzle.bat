@echo off

set pholdr=A25xx

set /p pzNm=Enter Puzzle Name (e.g. A25xx):

if "%pzNm%"=="" (
    goto ExitHere
)

if exist "AdventOfCode2025.cabal" (
    echo Cabal file found ...
)

set templPath="app\Puzzles\Parts\%pholdr%.hs" 
if not exist %templPath% (
    echo Template %templPath% not found
    goto ExitHere
)

set templInDir="app\Puzzles\Input\%pholdr%\" 
if not exist %templInDir% (
    echo Input template %templInDir% not found
    goto ExitHere
)


set pzInDir="app\Puzzles\Input\%pzNm%\"
if not exist %pzInDir% (
    echo Creating %pzInDir% ... 
    mkdir %pzInDir%
) else (
    echo %pzInDir% already exists and will not be created
)

echo Copying %templInDir% to %pzInDir% ... 
copy %templInDir% %pzInDir%

set pzPath="app\Puzzles\Parts\%pzNm%.hs"

if not exist %pzPath% (
    echo Creating %pzPath% from %templPath%
    copy %templPath% %pzPath%
) else (
    echo %pzPath% already exists and will not be created
)

Powershell -executionpolicy bypass ".\NextPuzzle.ps1" "%pzNm:"=%"

:ExitHere
pause
