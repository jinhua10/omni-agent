/**
 * 主题引擎错误边界 / Theme Engine Error Boundary
 *
 * 捕获主题引擎相关错误，防止整个应用崩溃
 * Catches theme engine errors to prevent entire app from crashing
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react';

class ThemeEngineErrorBoundary extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      hasError: false,
      error: null,
      errorInfo: null,
    };
  }

  static getDerivedStateFromError(error) {
    // 更新状态，下次渲染将显示回退UI / Update state so next render shows fallback UI
    return { hasError: true };
  }

  componentDidCatch(error, errorInfo) {
    // 记录错误信息 / Log error information
    console.error('❌ Theme Engine Error Boundary caught an error:', error, errorInfo);
    this.setState({
      error,
      errorInfo,
    });

    // 这里可以将错误发送到错误追踪服务 / Error can be sent to error tracking service here
    // Example: logErrorToService(error, errorInfo);
  }

  handleReset = () => {
    // 重置错误状态 / Reset error state
    this.setState({
      hasError: false,
      error: null,
      errorInfo: null,
    });

    // 重置主题到默认值 / Reset theme to default
    try {
      localStorage.setItem('uiTheme', 'modern');
      window.location.reload();
    } catch (e) {
      console.error('Failed to reset theme:', e);
    }
  };

  render() {
    if (this.state.hasError) {
      // 渲染回退UI / Render fallback UI
      return (
        <div style={{
          display: 'flex',
          flexDirection: 'column',
          alignItems: 'center',
          justifyContent: 'center',
          minHeight: '100vh',
          padding: '20px',
          background: 'linear-gradient(135deg, #667eea 0%, #764ba2 100%)',
          color: 'white',
          fontFamily: 'system-ui, -apple-system, sans-serif',
        }}>
          <div style={{
            background: 'rgba(255, 255, 255, 0.95)',
            borderRadius: '20px',
            padding: '40px',
            maxWidth: '600px',
            boxShadow: '0 20px 60px rgba(0, 0, 0, 0.3)',
            color: '#333',
          }}>
            <h1 style={{
              fontSize: '32px',
              marginBottom: '20px',
              color: '#d32f2f',
            }}>
              ⚠️ 主题引擎错误 / Theme Engine Error
            </h1>

            <p style={{
              fontSize: '18px',
              marginBottom: '20px',
              lineHeight: '1.6',
            }}>
              主题系统遇到了问题，但不用担心，您的数据是安全的。
              <br />
              <em>The theme system encountered an issue, but don't worry, your data is safe.</em>
            </p>

            <div style={{
              background: '#f5f5f5',
              padding: '15px',
              borderRadius: '8px',
              marginBottom: '20px',
              fontSize: '14px',
              fontFamily: 'monospace',
              overflow: 'auto',
              maxHeight: '200px',
            }}>
              <strong>错误详情 / Error Details:</strong>
              <pre style={{ margin: '10px 0 0 0', whiteSpace: 'pre-wrap' }}>
                {this.state.error && this.state.error.toString()}
              </pre>
            </div>

            <div style={{
              display: 'flex',
              gap: '15px',
              justifyContent: 'center',
            }}>
              <button
                onClick={this.handleReset}
                style={{
                  padding: '12px 30px',
                  fontSize: '16px',
                  background: '#667eea',
                  color: 'white',
                  border: 'none',
                  borderRadius: '8px',
                  cursor: 'pointer',
                  fontWeight: '600',
                  transition: 'all 0.3s ease',
                }}
                onMouseOver={(e) => e.target.style.background = '#5568d3'}
                onMouseOut={(e) => e.target.style.background = '#667eea'}
              >
                🔄 重置并刷新 / Reset & Reload
              </button>

              <button
                onClick={() => window.location.reload()}
                style={{
                  padding: '12px 30px',
                  fontSize: '16px',
                  background: '#4caf50',
                  color: 'white',
                  border: 'none',
                  borderRadius: '8px',
                  cursor: 'pointer',
                  fontWeight: '600',
                  transition: 'all 0.3s ease',
                }}
                onMouseOver={(e) => e.target.style.background = '#45a049'}
                onMouseOut={(e) => e.target.style.background = '#4caf50'}
              >
                🔃 重新加载 / Reload Page
              </button>
            </div>

            <p style={{
              marginTop: '30px',
              fontSize: '14px',
              color: '#666',
              textAlign: 'center',
            }}>
              如果问题持续存在，请联系技术支持。
              <br />
              <em>If the problem persists, please contact technical support.</em>
            </p>
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

export default ThemeEngineErrorBoundary;

