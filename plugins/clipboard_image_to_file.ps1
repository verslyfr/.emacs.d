param (
    [string] $filename = $(throw "filename parameter is required")
)

snippingtool.exe /clip

Sleep 5

Add-Type -AssemblyName System.Windows.Forms
if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {
    $image = [System.Windows.Forms.Clipboard]::GetImage()
    [System.Drawing.Bitmap]$image.Save($filename,[System.Drawing.Imaging.ImageFormat]::Png)
    Write-Output 'clipboard content saved as file'}
else {
    Write-Output 'clipboard does not contain image data'
}
