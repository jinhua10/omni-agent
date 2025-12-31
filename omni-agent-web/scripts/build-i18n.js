#!/usr/bin/env node

/**
 * 国际化消息构建脚本
 *
 * 功能：
 * 1. 读取 src/i18n-source/*.js 文件
 * 2. 转换为标准JSON
 * 3. 输出到 src/main/resources/i18n/*.json
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */

const fs = require('fs');
const path = require('path');

// 配置
const SOURCE_DIR = path.join(__dirname, '../src/i18n-source');
const TARGET_DIR = path.join(__dirname, '../src/main/resources/i18n');

// 确保目标目录存在
if (!fs.existsSync(TARGET_DIR)) {
  fs.mkdirSync(TARGET_DIR, { recursive: true });
  console.log(`✓ Created directory: ${TARGET_DIR}`);
}

/**
 * 处理单个文件
 */
function processFile(filename) {
  const sourcePath = path.join(SOURCE_DIR, filename);
  const targetFilename = filename.replace('.js', '.json');
  const targetPath = path.join(TARGET_DIR, targetFilename);

  console.log(`Processing: ${filename} -> ${targetFilename}`);

  try {
    // 读取JS文件
    const content = fs.readFileSync(sourcePath, 'utf-8');

    // 移除 export default 并移除注释
    let objectStr = content
      .replace(/export\s+default\s+/, '')
      .replace(/\/\*[\s\S]*?\*\//g, '')  // 移除多行注释
      .replace(/\/\/.*/g, '')             // 移除单行注释
      .trim();

    // 如果以分号结尾，移除它
    if (objectStr.endsWith(';')) {
      objectStr = objectStr.substring(0, objectStr.length - 1);
    }

    // 使用Function构造器安全执行（比eval更安全）
    const obj = new Function(`'use strict'; return (${objectStr})`)();

    // 转换为格式化的JSON
    const json = JSON.stringify(obj, null, 2);

    // 写入目标文件
    fs.writeFileSync(targetPath, json, 'utf-8');

    console.log(`✓ Generated: ${targetPath}`);

    // 统计信息
    const keys = countKeys(obj);
    console.log(`  → ${keys} message keys`);

  } catch (error) {
    console.error(`✗ Failed to process ${filename}:`, error.message);
    process.exit(1);
  }
}

/**
 * 递归统计对象中的叶子节点数量
 */
function countKeys(obj) {
  let count = 0;
  for (const key in obj) {
    if (typeof obj[key] === 'object' && obj[key] !== null) {
      count += countKeys(obj[key]);
    } else {
      count++;
    }
  }
  return count;
}

/**
 * 主函数
 */
function main() {
  console.log('=== Building i18n messages ===\n');

  // 检查源目录是否存在
  if (!fs.existsSync(SOURCE_DIR)) {
    console.error(`✗ Source directory not found: ${SOURCE_DIR}`);
    process.exit(1);
  }

  // 读取所有.js文件
  const files = fs.readdirSync(SOURCE_DIR)
    .filter(f => f.endsWith('.js'));

  if (files.length === 0) {
    console.log('No .js files found in', SOURCE_DIR);
    return;
  }

  // 处理每个文件
  files.forEach(processFile);

  console.log(`\n✓ Build completed! Generated ${files.length} file(s).`);
}

// 执行主函数
main();

