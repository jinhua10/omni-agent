import { defineConfig } from 'vite'
import react from '@vitejs/plugin-react'
import path from 'path'

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    react({
      // 使用经典的 JSX 运行时，兼容现有代码
      jsxRuntime: 'classic'
    })
  ],

  // 开发服务器配置
  server: {
    port: 3000,
    open: true,
    proxy: {
      // 代理 API 请求到后端 Spring Boot 服务
      '/api': {
        target: 'http://localhost:8080',
        changeOrigin: true,
        secure: false
      }
    }
  },

  // 构建配置
  build: {
    // 输出到标准的 dist 目录（用于独立部署）
    outDir: 'dist',
    emptyOutDir: true, // 清空输出目录
    sourcemap: false, // 生产环境禁用sourcemap，提高性能
    minify: 'terser', // 使用terser压缩
    terserOptions: {
      compress: {
        drop_console: true, // 移除所有console
        drop_debugger: true, // 移除debugger
        pure_funcs: [
          'console.log',
          'console.info',
          'console.debug',
          'console.trace'
        ] // 明确移除这些console方法
      },
      format: {
        comments: false // 移除注释
      }
    },
    rollupOptions: {
      output: {
        // 静态资源分类打包
        chunkFileNames: 'js/chunks/[name]-[hash].js',
        entryFileNames: 'js/[name]-[hash].js',
        assetFileNames: (assetInfo) => {
          const info = assetInfo.name.split('.')
          const ext = info[info.length - 1]
          if (/\.(png|jpe?g|svg|gif|tiff|bmp|ico)$/i.test(assetInfo.name)) {
            return `assets/images/[name]-[hash][extname]`
          } else if (/\.css$/i.test(assetInfo.name)) {
            return `assets/css/[name]-[hash][extname]`
          }
          return `assets/[ext]/[name]-[hash][extname]`
        },
        // 手动分包
        manualChunks: {
          'react-vendor': ['react', 'react-dom'],
          'antd-vendor': ['antd', '@ant-design/icons'],
          'markdown-vendor': ['react-markdown', 'remark-gfm', 'rehype-raw'],
          'syntax-vendor': ['react-syntax-highlighter', 'highlight.js']
        }
      }
    }
  },

  // 路径别名
  resolve: {
    alias: {
      '@': path.resolve(__dirname, './src'),
      '@components': path.resolve(__dirname, './src/components'),
      '@api': path.resolve(__dirname, './src/api'),
      '@hooks': path.resolve(__dirname, './src/hooks'),
      '@contexts': path.resolve(__dirname, './src/contexts'),
      '@utils': path.resolve(__dirname, './src/utils'),
      '@styles': path.resolve(__dirname, './src/styles'),
      '@assets': path.resolve(__dirname, './src/assets')
    }
  },

  // 优化选项
  optimizeDeps: {
    include: ['react', 'react-dom', 'axios', 'marked', 'highlight.js']
  }
})

