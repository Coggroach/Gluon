[CmdletBinding()]
param(
    [Parameter(Position=0,Mandatory=0)][String]$Command    
)

$Name = "gluon-exe";

#Set-ExecutionPolicy -Scope Process -ExecutionPolicy Unrestricted

function Start-Build {
    stack build
}

function Start-Execute {
    stack exec $Name
}

switch ($Command) {
    "Build" { Start-Build; }
    "Execute" { Start-Execute; }
    Default {
        Start-Build;
        Start-Execute;
    }
}
