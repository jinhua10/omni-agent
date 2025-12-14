/**
 * 梦幻气泡主题诊断工具 / Dreamy Bubble Theme Diagnostic Tool
 * 
 * 在浏览器控制台运行此脚本进行诊断
 * Run this script in browser console for diagnostics
 */

(function() {
  console.log('🔍 梦幻气泡主题诊断开始 / Dreamy Bubble Theme Diagnostics Starting...\n');
  
  const results = {
    passed: [],
    failed: [],
    warnings: []
  };
  
  // 1. 检查主题配置
  console.log('1️⃣ 检查主题配置...');
  const uiTheme = localStorage.getItem('ui_theme');
  if (uiTheme === 'bubble') {
    results.passed.push('✅ 当前主题已设置为 bubble');
    console.log('   ✅ 当前主题: bubble');
  } else {
    results.failed.push('❌ 当前主题不是 bubble，是: ' + (uiTheme || '未设置'));
    console.log('   ❌ 当前主题:', uiTheme || '未设置');
  }
  
  // 2. 检查主题配置对象
  console.log('\n2️⃣ 检查主题配置对象...');
  try {
    if (typeof UI_THEMES !== 'undefined' && UI_THEMES.bubble) {
      results.passed.push('✅ UI_THEMES.bubble 配置存在');
      console.log('   ✅ 主题配置:', UI_THEMES.bubble);
      
      if (UI_THEMES.bubble.shellMapping?.collaboration) {
        results.passed.push('✅ collaboration shellMapping 已配置');
        console.log('   ✅ shellMapping.collaboration 已配置');
      } else {
        results.failed.push('❌ collaboration shellMapping 未配置');
        console.log('   ❌ shellMapping.collaboration 未配置');
      }
    } else {
      results.failed.push('❌ UI_THEMES.bubble 配置不存在');
      console.log('   ❌ UI_THEMES.bubble 不存在');
    }
  } catch (e) {
    results.failed.push('❌ 无法访问 UI_THEMES: ' + e.message);
    console.log('   ❌ 错误:', e.message);
  }
  
  // 3. 检查CSS加载
  console.log('\n3️⃣ 检查CSS文件加载...');
  const stylesheets = Array.from(document.styleSheets);
  const bubbleCss = stylesheets.find(sheet => 
    sheet.href && sheet.href.includes('bubble-collaboration')
  );
  
  if (bubbleCss) {
    results.passed.push('✅ bubble-collaboration.css 已加载');
    console.log('   ✅ CSS文件:', bubbleCss.href);
  } else {
    results.warnings.push('⚠️ bubble-collaboration.css 可能未加载（动态加载时正常）');
    console.log('   ⚠️ CSS文件未在当前页面找到（可能是动态加载）');
  }
  
  // 4. 检查DOM结构
  console.log('\n4️⃣ 检查DOM结构...');
  const dreamUniverse = document.querySelector('.dream-bubble-universe');
  const particleCanvas = document.querySelector('.particle-canvas');
  const navOrbs = document.querySelectorAll('.nav-orb');
  
  if (dreamUniverse) {
    results.passed.push('✅ 梦幻气泡容器已渲染');
    console.log('   ✅ .dream-bubble-universe 已找到');
  } else {
    results.failed.push('❌ 梦幻气泡容器未找到 - 可能未切换到协作面板');
    console.log('   ❌ .dream-bubble-universe 未找到');
  }
  
  if (particleCanvas) {
    results.passed.push('✅ 粒子画布已渲染');
    console.log('   ✅ Canvas 元素已找到');
  } else if (!dreamUniverse) {
    results.warnings.push('⚠️ 粒子画布未找到 - 请先进入协作面板');
  } else {
    results.failed.push('❌ 粒子画布未找到');
  }
  
  if (navOrbs.length > 0) {
    results.passed.push(`✅ 导航球已渲染 (${navOrbs.length}个)`);
    console.log(`   ✅ 找到 ${navOrbs.length} 个导航球`);
  } else if (!dreamUniverse) {
    results.warnings.push('⚠️ 导航球未找到 - 请先进入协作面板');
  } else {
    results.failed.push('❌ 导航球未找到');
  }
  
  // 5. 检查浏览器兼容性
  console.log('\n5️⃣ 检查浏览器兼容性...');
  const features = {
    canvas: typeof HTMLCanvasElement !== 'undefined',
    backdropFilter: CSS.supports('backdrop-filter', 'blur(10px)'),
    transform3d: CSS.supports('transform-style', 'preserve-3d'),
    customProperties: CSS.supports('--custom', 'value')
  };
  
  if (features.canvas) {
    results.passed.push('✅ Canvas API 支持');
  } else {
    results.failed.push('❌ Canvas API 不支持');
  }
  
  if (features.backdropFilter) {
    results.passed.push('✅ backdrop-filter 支持');
  } else {
    results.warnings.push('⚠️ backdrop-filter 不支持 (玻璃效果会降级)');
  }
  
  if (features.transform3d) {
    results.passed.push('✅ 3D Transform 支持');
  } else {
    results.warnings.push('⚠️ 3D Transform 不支持');
  }
  
  console.log('   浏览器特性:', features);
  
  // 6. 性能检查
  console.log('\n6️⃣ 性能检查...');
  if (performance && performance.memory) {
    const memory = performance.memory;
    const memoryMB = (memory.usedJSHeapSize / 1024 / 1024).toFixed(2);
    console.log(`   📊 内存使用: ${memoryMB} MB`);
    
    if (memoryMB < 100) {
      results.passed.push('✅ 内存使用正常');
    } else {
      results.warnings.push('⚠️ 内存使用较高: ' + memoryMB + ' MB');
    }
  }
  
  // 输出总结
  console.log('\n' + '='.repeat(60));
  console.log('📊 诊断总结 / Diagnostic Summary\n');
  
  console.log('✅ 通过检查 (' + results.passed.length + ')');
  results.passed.forEach(msg => console.log('  ' + msg));
  
  if (results.warnings.length > 0) {
    console.log('\n⚠️ 警告 (' + results.warnings.length + ')');
    results.warnings.forEach(msg => console.log('  ' + msg));
  }
  
  if (results.failed.length > 0) {
    console.log('\n❌ 失败检查 (' + results.failed.length + ')');
    results.failed.forEach(msg => console.log('  ' + msg));
  }
  
  console.log('\n' + '='.repeat(60));
  
  // 提供修复建议
  if (results.failed.length > 0) {
    console.log('\n🔧 修复建议 / Fix Suggestions:\n');
    
    if (!dreamUniverse) {
      console.log('1. 确保已进入"协作网络"页面');
      console.log('2. 在页面中找到并点击"协作"或"Collaboration"菜单');
    }
    
    if (uiTheme !== 'bubble') {
      console.log('1. 点击主题切换器图标');
      console.log('2. 选择"梦幻气泡"主题');
      console.log('3. 点击"应用"按钮');
      console.log('\n或直接运行: localStorage.setItem("ui_theme", "bubble"); location.reload();');
    }
  }
  
  if (results.failed.length === 0 && dreamUniverse) {
    console.log('\n🎉 恭喜！梦幻气泡主题工作正常！');
    console.log('   Congratulations! Dreamy Bubble Theme is working!');
  }
  
  console.log('\n');
  
  return {
    passed: results.passed,
    warnings: results.warnings,
    failed: results.failed,
    score: results.passed.length / (results.passed.length + results.failed.length) * 100
  };
})();
