# OmniAgent 服务器部署完整指南

**服务器**: yumbo.top  
**前端目录**: `/root/UI`  
**后端端口**: `8080`  
**更新日期**: 2025-12-29

---

## 📋 部署架构

```
用户访问 yumbo.top (80端口)
    ↓
Nginx (反向代理)
    ├─→ 静态文件: /root/UI/dist/  (前端)
    └─→ API请求: http://127.0.0.1:8080  (后端Spring Boot)
```

---

## 🚀 完整部署步骤

### 步骤1: 准备服务器环境

```bash
# 更新系统
sudo apt update && sudo apt upgrade -y

# 安装必要软件
sudo apt install -y nginx git curl

# 安装Java 21 (后端需要)
sudo apt install -y openjdk-21-jdk

# 验证安装
java -version
nginx -v
```

---

### 步骤2: 部署前端

#### 2.1 本地构建前端

在您的**本地开发机器**上：

```bash
# 进入前端目录
cd D:\Jetbrains\omni-agent\UI

# 安装依赖（如果还没安装）
npm install

# 生产构建
npm run build

# 此时会在 UI/dist/ 目录生成构建产物
```

#### 2.2 上传到服务器

**方案A: 使用 SCP**

```bash
# 在本地执行
scp -r dist/* root@yumbo.top:/root/UI/dist/
```

**方案B: 使用 SFTP**

```bash
# 连接到服务器
sftp root@yumbo.top

# 上传整个dist目录
put -r dist /root/UI/
```

**方案C: 使用 rsync（推荐）**

```bash
# 同步dist目录，自动增量上传
rsync -avz --delete dist/ root@yumbo.top:/root/UI/dist/
```

#### 2.3 在服务器上创建目录

如果 `/root/UI` 目录不存在：

```bash
# SSH 登录服务器
ssh root@yumbo.top

# 创建前端目录
mkdir -p /root/UI/dist

# 设置权限
chmod -R 755 /root/UI/dist
```

---

### 步骤3: 部署后端

#### 3.1 上传后端代码

```bash
# 在本地项目根目录
# 方案1: 直接上传编译好的jar包
scp omni-agent-web/target/omni-agent-web-1.0.0.jar root@yumbo.top:/root/omni-agent/

# 方案2: 上传整个项目，在服务器上编译
rsync -avz --exclude 'target' --exclude 'node_modules' \
  . root@yumbo.top:/root/omni-agent/
```

#### 3.2 在服务器上构建（如果上传的是源代码）

```bash
# SSH 登录服务器
ssh root@yumbo.top

cd /root/omni-agent

# Maven 构建
./mvnw clean package -DskipTests

# jar包会在 omni-agent-web/target/ 目录
```

#### 3.3 配置后端

创建配置文件 `/root/omni-agent/application-prod.yml`:

```yaml
server:
  port: 8080
  
spring:
  profiles:
    active: prod

omni:
  document-storage:
    file:
      base-path: /root/omni-agent/data/storage
      
  rag:
    file:
      enabled: true
      index-path: /root/omni-agent/data/rag
      
logging:
  level:
    top.yumbo.ai.omni: INFO
  file:
    name: /root/omni-agent/logs/application.log
```

#### 3.4 启动后端服务

**方案A: 直接启动（测试用）**

```bash
cd /root/omni-agent/omni-agent-web/target

java -jar omni-agent-web-1.0.0.jar \
  --spring.config.location=/root/omni-agent/application-prod.yml
```

**方案B: 使用 systemd（生产推荐）**

创建服务文件 `/etc/systemd/system/omni-agent.service`:

```ini
[Unit]
Description=OmniAgent Backend Service
After=network.target

[Service]
Type=simple
User=root
WorkingDirectory=/root/omni-agent
ExecStart=/usr/bin/java -jar \
  -Xms512m -Xmx2g \
  -Dspring.config.location=/root/omni-agent/application-prod.yml \
  /root/omni-agent/omni-agent-web/target/omni-agent-web-1.0.0.jar
Restart=always
RestartSec=10
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
```

启动服务：

```bash
# 重载 systemd
sudo systemctl daemon-reload

# 启动服务
sudo systemctl start omni-agent

# 设置开机自启
sudo systemctl enable omni-agent

# 查看状态
sudo systemctl status omni-agent

# 查看日志
journalctl -u omni-agent -f
```

---

### 步骤4: 配置 Nginx

#### 4.1 备份原配置

```bash
sudo cp /etc/nginx/nginx.conf /etc/nginx/nginx.conf.backup
```

#### 4.2 更新配置

将我生成的 `nginx.conf` 文件内容复制到服务器：

```bash
# 编辑 Nginx 配置
sudo nano /etc/nginx/nginx.conf

# 或者直接上传配置文件
scp docs/nginx.conf root@yumbo.top:/etc/nginx/nginx.conf
```

#### 4.3 测试配置

```bash
# 测试配置语法
sudo nginx -t

# 如果显示 "syntax is ok" 和 "test is successful"，继续
```

#### 4.4 重启 Nginx

```bash
# 重启 Nginx
sudo systemctl restart nginx

# 设置开机自启
sudo systemctl enable nginx

# 查看状态
sudo systemctl status nginx
```

---

## ✅ 验证部署

### 1. 检查后端服务

```bash
# 检查8080端口是否监听
sudo netstat -tlnp | grep 8080

# 或使用 ss 命令
sudo ss -tlnp | grep 8080

# 测试后端API
curl http://localhost:8080/api/health
```

### 2. 检查前端文件

```bash
# 检查前端文件是否存在
ls -la /root/UI/dist/

# 应该看到：
# index.html
# js/
# assets/
```

### 3. 检查 Nginx

```bash
# 检查80端口
sudo netstat -tlnp | grep :80

# 测试Nginx配置
curl -I http://localhost
```

### 4. 浏览器测试

访问以下URL：
- **首页**: http://yumbo.top/ （应显示 Landing Page）
- **Demo**: http://yumbo.top/#/demo/qa （应显示主应用）
- **API测试**: http://yumbo.top/api/health

---

## 🔧 常用命令

### 后端服务管理

```bash
# 启动
sudo systemctl start omni-agent

# 停止
sudo systemctl stop omni-agent

# 重启
sudo systemctl restart omni-agent

# 查看状态
sudo systemctl status omni-agent

# 查看日志
journalctl -u omni-agent -f

# 查看最近100行日志
journalctl -u omni-agent -n 100
```

### Nginx 管理

```bash
# 启动
sudo systemctl start nginx

# 停止
sudo systemctl stop nginx

# 重启
sudo systemctl restart nginx

# 重新加载配置（不中断服务）
sudo systemctl reload nginx

# 测试配置
sudo nginx -t

# 查看日志
sudo tail -f /var/log/nginx/omni-agent-access.log
sudo tail -f /var/log/nginx/omni-agent-error.log
```

---

## 🔄 更新部署

### 更新前端

```bash
# 本地构建
cd D:\Jetbrains\omni-agent\UI
npm run build

# 上传到服务器
rsync -avz --delete dist/ root@yumbo.top:/root/UI/dist/

# 清除浏览器缓存后访问
```

### 更新后端

```bash
# 本地构建
cd D:\Jetbrains\omni-agent
mvn clean package -DskipTests

# 上传jar包
scp omni-agent-web/target/omni-agent-web-1.0.0.jar \
  root@yumbo.top:/root/omni-agent/omni-agent-web/target/

# 在服务器上重启服务
ssh root@yumbo.top "sudo systemctl restart omni-agent"
```

---

## 🐛 故障排查

### 问题1: 访问 yumbo.top 显示 502 Bad Gateway

**原因**: 后端服务未启动或8080端口未监听

**解决**:
```bash
# 检查后端服务状态
sudo systemctl status omni-agent

# 检查端口
sudo netstat -tlnp | grep 8080

# 查看后端日志
journalctl -u omni-agent -n 50
```

### 问题2: 前端页面是空白的

**原因**: 前端文件未正确上传或路径配置错误

**解决**:
```bash
# 检查文件是否存在
ls -la /root/UI/dist/index.html

# 检查Nginx配置
sudo nginx -t

# 查看Nginx错误日志
sudo tail -f /var/log/nginx/omni-agent-error.log
```

### 问题3: API 请求404或跨域错误

**原因**: Nginx代理配置问题

**解决**:
```bash
# 检查Nginx配置中的 location /api 部分
sudo nginx -t

# 查看Nginx访问日志
sudo tail -f /var/log/nginx/omni-agent-access.log
```

### 问题4: 刷新页面出现404

**原因**: SPA路由配置问题

**解决**: 确保Nginx配置中有：
```nginx
location / {
    try_files $uri $uri/ /index.html;
}
```

---

## 🔐 安全加固

### 1. 配置防火墙

```bash
# 启用UFW
sudo ufw enable

# 允许SSH
sudo ufw allow 22

# 允许HTTP
sudo ufw allow 80

# 允许HTTPS（如果配置了SSL）
sudo ufw allow 443

# 查看规则
sudo ufw status
```

### 2. 配置 HTTPS（推荐）

```bash
# 安装 Certbot
sudo apt install -y certbot python3-certbot-nginx

# 获取SSL证书
sudo certbot --nginx -d yumbo.top -d www.yumbo.top

# 自动续期
sudo certbot renew --dry-run
```

Certbot 会自动修改Nginx配置，添加HTTPS支持。

### 3. 限制后端8080端口访问

编辑 `/etc/systemd/system/omni-agent.service`，添加：

```ini
[Service]
# 只监听本地地址
Environment="SERVER_ADDRESS=127.0.0.1"
```

---

## 📊 监控和日志

### 查看系统资源

```bash
# 查看内存使用
free -h

# 查看磁盘使用
df -h

# 查看CPU和内存（实时）
htop

# 查看Java进程
ps aux | grep java
```

### 日志位置

- **后端日志**: `/root/omni-agent/logs/application.log`
- **Nginx访问日志**: `/var/log/nginx/omni-agent-access.log`
- **Nginx错误日志**: `/var/log/nginx/omni-agent-error.log`
- **系统日志**: `journalctl -u omni-agent`

---

## 📝 快速命令参考

```bash
# 一键重启所有服务
sudo systemctl restart omni-agent nginx

# 查看所有服务状态
sudo systemctl status omni-agent nginx

# 实时查看所有日志
sudo tail -f /var/log/nginx/omni-agent-access.log \
            /root/omni-agent/logs/application.log

# 清理日志（如果磁盘空间不足）
sudo journalctl --vacuum-time=7d
```

---

## 🎉 部署检查清单

- [ ] Java 21 已安装
- [ ] Nginx 已安装
- [ ] 前端文件已上传到 `/root/UI/dist/`
- [ ] 后端jar包已上传
- [ ] 后端配置文件已创建
- [ ] systemd 服务已配置
- [ ] 后端服务已启动（8080端口监听）
- [ ] Nginx 配置已更新
- [ ] Nginx 已重启
- [ ] 访问 http://yumbo.top 显示首页
- [ ] 访问 http://yumbo.top/#/demo/qa 显示主应用
- [ ] API 请求正常
- [ ] 防火墙已配置
- [ ] （可选）HTTPS 已配置

---

**部署支持**: 
- GitHub: https://github.com/jinhua10/omni-agent
- 邮箱: 1015770492@qq.com

