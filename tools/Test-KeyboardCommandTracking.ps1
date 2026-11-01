[CmdletBinding()]
param(
    [Parameter(Mandatory = $true)]
    [string]$ExecutablePath
)

$ErrorActionPreference = 'Stop'
if ($PSVersionTable.PSEdition -ne 'Desktop') {
    throw 'Run this test with Windows PowerShell (powershell.exe), not pwsh.exe.'
}
$exe = (Resolve-Path -LiteralPath $ExecutablePath).ProviderPath
Add-Type -AssemblyName System.Windows.Forms
[void][Reflection.Assembly]::LoadFrom($exe)

# Exercise the real callback without constructing a tray icon, installing a hook,
# sending input, or starting the application's normal configuration/file loading.
$testSource = @'
using System;
using System.Reflection;
using System.Runtime.Serialization;
using uclliu;

public static class KeyboardCommandRegression
{
    public static void Run()
    {
        const BindingFlags PrivateInstance = BindingFlags.Instance | BindingFlags.NonPublic;
        Form1 form = (Form1)FormatterServices.GetUninitializedObject(typeof(Form1));
        global::uclliu.uclliu app = new global::uclliu.uclliu(ref form);
        try
        {
            app.flag_is_ucl = false;
            app.flag_is_hf = false;
            app.sendkey_not_use_ucl_apps.Clear();
            app.config["DEFAULT"]["CTRL_SP"] = "1";
            app.config["DEFAULT"]["KEYBOARD_VOLUME"] = "0";
            typeof(Form1).GetField("ucl", PrivateInstance).SetValue(form, app);
            typeof(Form1).GetField("ctrlSpaceShortcut", PrivateInstance).SetValue(form, new CtrlSpaceShortcutState());
            KeyboardConsumedKeys consumed = new KeyboardConsumedKeys();
            typeof(Form1).GetField("fullWidthKeys", PrivateInstance).SetValue(form, consumed);
            MethodInfo callback = typeof(Form1).GetMethod("LowLevelKeyboardProc", PrivateInstance);
            Type payloadType = typeof(Form1).GetNestedType("KBDLLHOOKSTRUCT", BindingFlags.NonPublic);

            // Stop one character before VERSION so this test cannot open About UI.
            int[] keys = { 188, 188, 188, 86, 69, 82, 83, 73, 79 };
            const string expected = ",,,versio";
            for (int i = 0; i < keys.Length; i++)
            {
                consumed.MarkKeyDown(keys[i]);
                object payload = Activator.CreateInstance(payloadType);
                payloadType.GetField("vkCode").SetValue(payload, keys[i]);
                object[] arguments = { 0, 0x0101, payload };
                int result = (int)callback.Invoke(form, arguments);
                if (result != 1)
                    throw new Exception("Converted keyup was not consumed at index " + i);
                string prefix = expected.Substring(0, i + 1);
                if (app.last_key != prefix)
                    throw new Exception("Command tracking lost converted keyup: expected '" + prefix + "', got '" + app.last_key + "'.");
            }
        }
        finally
        {
            app.shutdown_output_dispatcher();
        }
    }
}
'@
$compiler = New-Object Microsoft.CSharp.CSharpCodeProvider
$compilerOptions = New-Object System.CodeDom.Compiler.CompilerParameters
$compilerOptions.GenerateInMemory = $true
foreach ($reference in @($exe, 'System.dll', 'System.Windows.Forms.dll', 'System.Drawing.dll', 'System.Web.Extensions.dll')) {
    [void]$compilerOptions.ReferencedAssemblies.Add($reference)
}
try {
    $compiled = $compiler.CompileAssemblyFromSource($compilerOptions, $testSource)
    if ($compiled.Errors.HasErrors) {
        throw ($compiled.Errors | ForEach-Object { $_.ToString() } | Out-String)
    }
    [void]$compiled.CompiledAssembly.GetType('KeyboardCommandRegression').GetMethod('Run').Invoke($null, @())
}
finally {
    $compiler.Dispose()
}
Write-Output 'PASS: converted keyup remains consumed after tracking extra-command prefix'
