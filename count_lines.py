import os
import sys

# 根据pom.xml中的模块列表
modules = [
    'omni-agent-document-storage-api',
    'omni-agent-rag-api',
    'omni-agent-ai-api',
    'omni-agent-p2p-api',
    'omni-agent-knowledge-registry-api',
    'omni-agent-chunking-api',
    'omni-agent-document-processor-api',
    'omni-agent-common',
    'omni-agent-core',
    'omni-agent-chunking-starter',
    'omni-agent-document-processor-starter',
    'omni-agent-web',
    'omni-agent-marketplace',
    'omni-agent-workflow',
    'omni-agent-document-storage-starter',
    'omni-agent-ocr-starter-tesseract',
    'omni-agent-rag-starter-adapter',
    'omni-agent-ai-starter',
    'omni-agent-knowledge-registry-starter',
    'omni-agent-example-basic',
    'omni-agent-example-production',
    'omni-agent-p2p-starter'
]

total_lines = 0
total_files = 0

print("=" * 80)
print("统计各模块Java代码行数")
print("=" * 80)

for module in modules:
    java_dir = os.path.join(module, 'src', 'main', 'java')

    if not os.path.exists(java_dir):
        print(f"{module:45} - 不存在")
        continue

    module_lines = 0
    module_files = 0

    for root, dirs, files in os.walk(java_dir):
        for file in files:
            if file.endswith('.java'):
                file_path = os.path.join(root, file)
                try:
                    with open(file_path, 'r', encoding='utf-8', errors='ignore') as f:
                        lines = len(f.readlines())
                        module_lines += lines
                        module_files += 1
                except Exception as e:
                    print(f"  错误读取: {file_path} - {e}")

    total_lines += module_lines
    total_files += module_files
    print(f"{module:45} {module_files:4} 文件  {module_lines:7} 行")

print("=" * 80)
print(f"{'总计':45} {total_files:4} 文件  {total_lines:7} 行")
print("=" * 80)

