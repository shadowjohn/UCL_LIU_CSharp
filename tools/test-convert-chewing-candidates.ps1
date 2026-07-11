[CmdletBinding()]
param()

$ErrorActionPreference = "Stop"
$converter = Join-Path $PSScriptRoot "convert-chewing-candidates.ps1"
$tempRoot = Join-Path ([System.IO.Path]::GetTempPath()) ("uclliu-candidate-test-" + [Guid]::NewGuid().ToString("N"))
$inputPath = Join-Path $tempRoot "tsi.csv"
$output1 = Join-Path $tempRoot "candidate-1.txt"
$output2 = Join-Path $tempRoot "candidate-2.txt"

try {
    New-Item -ItemType Directory -Path $tempRoot | Out-Null
    $extensionB = [Char]::ConvertFromUtf32(0x20000)
    $csv = @(
        "# dc:title,測試詞庫,"
        "# dc:license,LGPL-2.1-or-later,"
        '"王小明",100,"ㄨㄤˊ ㄒㄧㄠˇ ㄇㄧㄥˊ"'
        '"王先生",50,"ㄨㄤˊ ㄒㄧㄢ ㄕㄥ"'
        '"王小明",200,"duplicate, quoted phone"'
        ('"' + $extensionB + '中華",80,"supplementary"')
        '極大,9223372036854775807,max long'
        '極小,-9223372036854775808,min long'
        '壞資料,not-a-number,invalid'
        ',30,missing phrase'
        '單,10,too short'
    )
    [System.IO.File]::WriteAllLines($inputPath, $csv, [System.Text.UTF8Encoding]::new($false))

    $log1 = @(& $converter -InputPath $inputPath -OutputPath $output1 *>&1) -join "`n"
    & $converter -InputPath $inputPath -OutputPath $output2 *> $null

    $hash1 = (Get-FileHash -LiteralPath $output1 -Algorithm SHA256).Hash
    $hash2 = (Get-FileHash -LiteralPath $output2 -Algorithm SHA256).Hash
    if ($hash1 -ne $hash2) { throw "相同輸入的 SHA256 不一致。" }

    $lines = [System.IO.File]::ReadAllLines($output1, [System.Text.Encoding]::UTF8)
    $wang = $lines | Where-Object { $_.StartsWith("王`t", [System.StringComparison]::Ordinal) } | Select-Object -First 1
    if ($wang -ne "王`t小明`t先生") { throw "王的候選排序錯誤：$wang" }
    if (-not ($lines -contains ($extensionB + "`t中華"))) { throw "補充平面字元被拆壞。" }
    if (-not ($lines -contains ($extensionB + "中`t華"))) { throw "補充平面前綴被拆壞。" }
    if (-not ($lines -contains "極`t大`t小")) { throw "64-bit 頻率排序錯誤。" }
    if ($lines -match "壞資料|missing phrase|too short") { throw "無效資料不應輸出。" }
    if ($log1 -notmatch "Metadata rows:\s*2" -or $log1 -notmatch "Invalid rows:\s*2" -or
        $log1 -notmatch "Short phrases:\s*1") {
        throw "略過資料的計數不正確：$log1"
    }

    $bytes = [System.IO.File]::ReadAllBytes($output1)
    if ($bytes.Length -ge 3 -and $bytes[0] -eq 0xEF -and $bytes[1] -eq 0xBB -and $bytes[2] -eq 0xBF) {
        throw "輸出不應包含 UTF-8 BOM。"
    }

    Write-Host "PASS converter SHA256=$hash1"
} finally {
    if (Test-Path -LiteralPath $tempRoot) {
        Remove-Item -LiteralPath $tempRoot -Recurse -Force
    }
}
