# OmniAgent 后端启动脚本（Windows）
# 修复中文乱码问题和路径问题

# 获取脚本所在目录
$SCRIPT_DIR = Split-Path -Parent $MyInvocation.MyCommand.Path
# 项目根目录（scripts的上级目录）
$PROJECT_ROOT = Split-Path -Parent $SCRIPT_DIR

# JAR包路径
$JAR_NAME = "omni-agent-example-basic/target/omni-agent-example-basic-1.0.0.jar"
$JAR_PATH = Join-Path $PROJECT_ROOT $JAR_NAME

# 端口配置
$SERVER_PORT = if ($env:SERVER_PORT) { $env:SERVER_PORT } else { 8080 }

# 日志文件（保存在项目根目录）
$LOG_FILE = Join-Path $PROJECT_ROOT "app.log"

Write-Host "======================================"
Write-Host "🚀 启动 OmniAgent 后端服务" -ForegroundColor Green
Write-Host "======================================"
Write-Host "项目根目录: $PROJECT_ROOT"
Write-Host "JAR包路径: $JAR_PATH"
Write-Host "工作目录: $PROJECT_ROOT (切换后)"
Write-Host "端口: $SERVER_PORT"
Write-Host "编码: UTF-8"
Write-Host "日志: $LOG_FILE"
Write-Host "======================================"
Write-Host ""

# 检查JAR包是否存在
if (-not (Test-Path $JAR_PATH)) {
    Write-Host "❌ 错误: 找不到JAR包" -ForegroundColor Red
    Write-Host "   期望位置: $JAR_PATH" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "请先编译项目:" -ForegroundColor Yellow
    Write-Host "   cd $PROJECT_ROOT"
    Write-Host "   mvn clean package -pl omni-agent-example-basic -am -DskipTests"
    exit 1
}

# ⭐ 重要：切换到项目根目录（确保 ./models, ./data 等相对路径正确）
Write-Host "📂 切换工作目录到项目根目录..." -ForegroundColor Yellow
Set-Location $PROJECT_ROOT
Write-Host "   当前工作目录: $(Get-Location)" -ForegroundColor Cyan
Write-Host ""

# 验证关键目录
if (Test-Path ".\models") {
    Write-Host "✅ ./models 目录存在" -ForegroundColor Green
} else {
    Write-Host "⚠️  ./models 目录不存在，ONNX模型功能可能不可用" -ForegroundColor Yellow
}

if (Test-Path ".\data") {
    Write-Host "✅ ./data 目录存在" -ForegroundColor Green
} else {
    Write-Host "ℹ️  ./data 目录不存在，将在首次运行时自动创建" -ForegroundColor Cyan
}
Write-Host ""

# 设置控制台编码为UTF-8
Write-Host "📝 设置UTF-8编码..." -ForegroundColor Yellow
[Console]::OutputEncoding = [System.Text.Encoding]::UTF8
chcp 65001 | Out-Null

Write-Host ""
Write-Host "✅ 准备就绪，正在启动..." -ForegroundColor Green
Write-Host "   ./models → $PROJECT_ROOT\models"
Write-Host "   ./data   → $PROJECT_ROOT\data"
Write-Host ""

# 启动应用
# 使用数组方式构建参数，避免PowerShell反引号问题
$javaArgs = @(
    "-Dfile.encoding=UTF-8",
    "-Dsun.jnu.encoding=UTF-8",
    "-Dconsole.encoding=UTF-8",
    "-jar",
    $JAR_PATH,
    "--server.port=$SERVER_PORT",
    "--logging.charset.console=UTF-8",
    "--logging.charset.file=UTF-8"
)

# 启动Java进程
& java $javaArgs 2>&1 | Tee-Object -FilePath $LOG_FILE

