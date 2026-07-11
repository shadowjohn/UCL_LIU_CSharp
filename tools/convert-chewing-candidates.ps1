[CmdletBinding()]
param(
    [Parameter(Mandatory)][string]$InputPath,
    [Parameter(Mandatory)][string]$OutputPath
)

$ErrorActionPreference = "Stop"
$rows = [System.Collections.Generic.Dictionary[string, object]]::new([System.StringComparer]::Ordinal)
$total = 0
$metadata = 0
$invalid = 0
$short = 0
$mappingCount = 0

function Split-UnicodeScalars {
    param([string]$Text)

    $scalars = [System.Collections.Generic.List[string]]::new()
    for ($i = 0; $i -lt $Text.Length; $i++) {
        $ch = $Text[$i]
        if ([Char]::IsHighSurrogate($ch)) {
            if ($i + 1 -ge $Text.Length -or -not [Char]::IsLowSurrogate($Text[$i + 1])) {
                throw "unpaired high surrogate"
            }
            $scalars.Add($Text.Substring($i, 2))
            $i++
        } elseif ([Char]::IsLowSurrogate($ch)) {
            throw "unpaired low surrogate"
        } else {
            $scalars.Add([string]$ch)
        }
    }
    return $scalars.ToArray()
}

$csvText = [System.IO.File]::ReadAllText($InputPath, [System.Text.UTF8Encoding]::new($false, $true))
$csvOffset = 0
while ($metadata -lt 4 -and $csvOffset -lt $csvText.Length -and $csvText[$csvOffset] -eq '#') {
    $lineEnd = $csvText.IndexOf("`n", $csvOffset)
    if ($lineEnd -lt 0) {
        $csvOffset = $csvText.Length
    } else {
        $csvOffset = $lineEnd + 1
    }
    $metadata++
}
$records = @(ConvertFrom-Csv -InputObject $csvText.Substring($csvOffset) -Header Phrase,Frequency,Phone)
$total = $metadata + $records.Count
$recordNumber = $metadata
foreach ($record in $records) {
    $recordNumber++
    $phrase = [string]$record.Phrase

    $frequency = 0L
    if ([string]::IsNullOrEmpty($phrase) -or $phrase.IndexOfAny(@("`t", "`r", "`n")) -ge 0 -or
        -not [long]::TryParse([string]$record.Frequency, [System.Globalization.NumberStyles]::Integer,
            [System.Globalization.CultureInfo]::InvariantCulture, [ref]$frequency)) {
        $invalid++
        Write-Warning "Skipping invalid CSV record $recordNumber."
        continue
    }

    try {
        $scalars = @(Split-UnicodeScalars $phrase)
    } catch {
        $invalid++
        Write-Warning "Skipping invalid Unicode in CSV record $recordNumber."
        continue
    }
    if ($scalars.Count -lt 2) {
        $short++
        continue
    }

    $maxPrefix = [Math]::Min(3, $scalars.Count - 1)
    for ($length = 1; $length -le $maxPrefix; $length++) {
        $key = [string]::Concat($scalars[0..($length - 1)])
        $suffix = [string]::Concat($scalars[$length..($scalars.Count - 1)])
        if (-not $rows.ContainsKey($key)) {
            $rows[$key] = [System.Collections.Generic.Dictionary[string, long]]::new([System.StringComparer]::Ordinal)
        }
        if (-not $rows[$key].ContainsKey($suffix) -or $rows[$key][$suffix] -lt $frequency) {
            $rows[$key][$suffix] = $frequency
        }
    }
}

$keys = [string[]]@($rows.Keys)
[Array]::Sort($keys, [System.StringComparer]::Ordinal)
$lines = [System.Collections.Generic.List[string]]::new()
$candidateComparer = [System.Collections.Generic.Comparer[object]]::Create({
    param($left, $right)
    $frequencyOrder = [long]$right.Value - [long]$left.Value
    if ($frequencyOrder -lt 0) { return -1 }
    if ($frequencyOrder -gt 0) { return 1 }
    return [System.StringComparer]::Ordinal.Compare([string]$left.Key, [string]$right.Key)
})

foreach ($key in $keys) {
    $entries = [object[]]@($rows[$key].GetEnumerator())
    [Array]::Sort($entries, $candidateComparer)
    $candidates = [string[]]@($entries | ForEach-Object { [string]$_.Key })
    $lines.Add($key + "`t" + ($candidates -join "`t"))
    $mappingCount += $candidates.Count
}

[System.IO.File]::WriteAllLines($OutputPath, $lines, [System.Text.UTF8Encoding]::new($false))
Write-Host "CSV rows:       $total"
Write-Host "Metadata rows:  $metadata"
Write-Host "Invalid rows:   $invalid"
Write-Host "Short phrases:  $short"
Write-Host "Candidate keys: $($keys.Count)"
Write-Host "Mappings:       $mappingCount"
