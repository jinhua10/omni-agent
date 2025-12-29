# Nginx "连接被拒绝" 问题完整解决方案

## 🐛 问题现象

访问 `yumbo.top` 时显示：**拒绝了我们的连接请求**

## 🔍 可能的原因

### 1️⃣ Nginx未启动（最常见）

**检查**:
```bash
sudo systemctl status nginx
```

**修复**:
```bash
# 启动Nginx
sudo systemctl start nginx

# 设置开机自启
sudo systemctl enable nginx
```

### 2️⃣ 80端口未监听

**检查**:
```bash
sudo netstat -tlnp | grep :80
# 或
sudo ss -tlnp | grep :80
```

**原因**:
- Nginx配置错误
- 端口被其他程序占用

**修复**:
```bash
# 测试配置
sudo nginx -t

# 如果配置正确但端口未监听，查看错误日志
sudo tail -f /var/log/nginx/error.log
```

### 3️⃣ 防火墙阻止（很常见）

**检查 UFW (Ubuntu/Debian)**:
```bash
sudo ufw status
```

**修复**:
```bash
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp
sudo ufw reload
```

**检查 Firewalld (CentOS/RHEL)**:
```bash
sudo firewall-cmd --list-ports
```

**修复**:
```bash
sudo firewall-cmd --permanent --add-service=http
sudo firewall-cmd --permanent --add-service=https
sudo firewall-cmd --reload
```

### 4️⃣ 权限问题

**检查前端文件权限**:
```bash
ls -la /var/www/omni-agent/ui
```

**修复**:
```bash
# 设置正确的所有权
sudo chown -R www-data:www-data /var/www/omni-agent

# 设置正确的权限
sudo chmod -R 755 /var/www/omni-agent
sudo find /var/www/omni-agent -type f -exec chmod 644 {} \;
```

### 5️⃣ SELinux阻止（CentOS/RHEL）

**检查**:
```bash
getenforce
```

**如果返回 `Enforcing`，修复**:
```bash
# 允许Nginx网络连接
sudo setsebool -P httpd_can_network_connect 1

# 设置文件上下文
sudo chcon -R -t httpd_sys_content_t /var/www/omni-agent

# 允许Nginx代理
sudo setsebool -P httpd_can_network_relay 1
```

### 6️⃣ 配置文件路径错误

**问题**: 配置文件中 `root /var/www/omni-agent/ui;` 但实际文件在 `/root/UI/dist`

**检查**:
```bash
# 查看配置
grep "root " /etc/nginx/nginx.conf

# 检查目录是否存在
ls -la /var/www/omni-agent/ui
```

**修复**:
```bash
# 方式1: 移动文件到正确位置
sudo mkdir -p /var/www/omni-agent/ui
sudo cp -r /root/UI/dist/* /var/www/omni-agent/ui/
sudo chown -R www-data:www-data /var/www/omni-agent

# 方式2: 修改配置文件指向实际路径（不推荐）
# 编辑配置文件，将 root 改为 /root/UI/dist
```

### 7️⃣ 云服务器安全组未开放

**适用于**: 阿里云、腾讯云、AWS等

**检查**:
- 登录云服务商控制台
- 查看安全组规则
- 确认80和443端口已开放

**修复**:
- 添加入站规则：允许TCP 80端口
- 添加入站规则：允许TCP 443端口
- 来源：0.0.0.0/0（所有IP）

## 🚀 一键诊断和修复

### 步骤1: 上传脚本到服务器

```bash
# 在本地
scp scripts/diagnose-nginx.sh root@yumbo.top:/tmp/
scp scripts/fix-nginx-permissions.sh root@yumbo.top:/tmp/
```

### 步骤2: 运行诊断脚本

```bash
# SSH登录服务器
ssh root@yumbo.top

# 运行诊断
sudo bash /tmp/diagnose-nginx.sh
```

### 步骤3: 根据诊断结果修复

```bash
# 运行一键修复脚本
sudo bash /tmp/fix-nginx-permissions.sh
```

## 📋 手动排查步骤

### 1. 检查Nginx状态

```bash
# 查看服务状态
sudo systemctl status nginx

# 查看进程
ps aux | grep nginx

# 查看监听端口
sudo netstat -tlnp | grep nginx
```

**预期结果**:
```
tcp        0      0 0.0.0.0:80              0.0.0.0:*               LISTEN      1234/nginx: master
```

### 2. 测试配置文件

```bash
sudo nginx -t
```

**预期结果**:
```
nginx: the configuration file /etc/nginx/nginx.conf syntax is ok
nginx: configuration file /etc/nginx/nginx.conf test is successful
```

### 3. 查看错误日志

```bash
# Nginx错误日志
sudo tail -f /var/log/nginx/error.log

# OmniAgent专用日志
sudo tail -f /var/log/nginx/omni-agent-error.log
```

### 4. 本地测试

```bash
# 在服务器上测试
curl -I http://localhost

# 预期返回
HTTP/1.1 200 OK
```

### 5. 远程测试

```bash
# 在本地测试
curl -I http://yumbo.top

# 如果还是连接被拒绝，检查防火墙和安全组
```

## 🛠️ 完整修复流程

```bash
# 1. 部署配置文件
sudo cp nginx-production.conf /etc/nginx/nginx.conf

# 2. 创建目录
sudo mkdir -p /var/www/omni-agent/ui

# 3. 上传前端文件
sudo rsync -avz /root/UI/dist/ /var/www/omni-agent/ui/

# 4. 设置权限
sudo chown -R www-data:www-data /var/www/omni-agent
sudo chmod -R 755 /var/www/omni-agent
sudo find /var/www/omni-agent -type f -exec chmod 644 {} \;

# 5. 配置SELinux（如果有）
sudo setsebool -P httpd_can_network_connect 1
sudo chcon -R -t httpd_sys_content_t /var/www/omni-agent

# 6. 配置防火墙
sudo ufw allow 80/tcp
sudo ufw allow 443/tcp

# 7. 测试配置
sudo nginx -t

# 8. 重启Nginx
sudo systemctl restart nginx
sudo systemctl enable nginx

# 9. 验证
curl -I http://localhost
curl -I http://yumbo.top
```

## ✅ 验证清单

- [ ] Nginx进程正在运行
- [ ] 80端口正在监听 (0.0.0.0:80)
- [ ] 配置文件测试通过 (nginx -t)
- [ ] 前端目录存在: `/var/www/omni-agent/ui`
- [ ] index.html文件存在
- [ ] 文件所有权正确: `www-data:www-data`
- [ ] 目录权限: 755
- [ ] 文件权限: 644
- [ ] 防火墙已开放80端口
- [ ] 安全组已开放80端口（云服务器）
- [ ] SELinux已配置（如果启用）
- [ ] 本地测试成功: `curl http://localhost`
- [ ] 远程测试成功: `curl http://yumbo.top`

## 🔧 常见错误和解决方案

### 错误1: Permission denied

```
nginx: [emerg] open() "/var/www/omni-agent/ui/index.html" failed (13: Permission denied)
```

**原因**: 文件权限或所有权不正确

**解决**:
```bash
sudo chown -R www-data:www-data /var/www/omni-agent
sudo chmod -R 755 /var/www/omni-agent
```

### 错误2: No such file or directory

```
nginx: [emerg] open() "/var/www/omni-agent/ui/index.html" failed (2: No such file or directory)
```

**原因**: 前端文件未上传或路径错误

**解决**:
```bash
# 检查文件是否存在
ls -la /var/www/omni-agent/ui/index.html

# 如果不存在，上传前端文件
```

### 错误3: Address already in use

```
nginx: [emerg] bind() to 0.0.0.0:80 failed (98: Address already in use)
```

**原因**: 80端口被占用

**解决**:
```bash
# 查看占用端口的进程
sudo lsof -i :80

# 停止其他Web服务
sudo systemctl stop apache2  # 如果是Apache
```

### 错误4: SELinux is preventing nginx

**解决**:
```bash
sudo ausearch -c 'nginx' --raw | audit2allow -M my-nginx
sudo semodule -i my-nginx.pp
```

## 📊 测试命令汇总

```bash
# 服务器端测试
curl -I http://localhost
curl http://localhost | head

# 本地测试
curl -I http://yumbo.top
ping yumbo.top

# 查看监听
sudo netstat -tlnp | grep :80

# 查看进程
ps aux | grep nginx

# 查看日志
sudo tail -f /var/log/nginx/error.log
sudo tail -f /var/log/nginx/omni-agent-access.log
```

## 💡 最终检查

如果以上所有都正常，但外部仍无法访问：

1. **DNS问题**: 检查域名解析
   ```bash
   nslookup yumbo.top
   dig yumbo.top
   ```

2. **云服务商安全组**: 登录控制台检查

3. **网络运营商屏蔽**: 尝试使用4G网络访问

4. **域名备案**: 中国大陆服务器需要ICP备案

---

**按照以上步骤逐一排查，问题一定能解决！**

