[CmdletBinding()]
param(
    [Parameter(Position=0,Mandatory=0)][String[]]$Command    
)

$Name = "gluon-exe";

#Set-ExecutionPolicy -Scope Process -ExecutionPolicy Unrestricted

function Start-Build {
    clear
    stack build
}

function Start-Execute ($t) {
    stack exec $Name $t
}

function Update-Dependancies {
    cabal install
}

if($Command -eq $null) {
    Write-Host "No Parameters: Exitting..."
    return;
}

switch ($Command[0]) {
    "Build" { Start-Build; }
    "Execute" { 
        if($Command.Length > 1) {
            Start-Execute $Command[1]; 
        } else {
            Start-Execute
        }
    }
    "Update" { Update-Dependancies; }
    Default {
        Start-Build;
        Start-Execute;
    }
}
