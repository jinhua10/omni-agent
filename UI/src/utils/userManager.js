/**
 * 用户管理工具 (User Manager Utility)
 * 
 * 管理用户唯一标识，用于区分不同机器/用户的对话历史
 * Manages user unique identifier for distinguishing conversation history
 * 
 * @author AI Reviewer Team
 * @since 2025-12-25
 */

const USER_ID_KEY = 'omni_agent_user_id'
const USER_INFO_KEY = 'omni_agent_user_info'

/**
 * 获取用户ID
 * 优先级：localStorage -> 服务器生成 -> 本地生成
 */
export async function getUserId() {
  // 1. 尝试从 localStorage 读取
  let userId = localStorage.getItem(USER_ID_KEY)
  
  if (userId) {
    console.log('📌 使用已存储的用户ID:', userId)
    return userId
  }

  // 2. 从服务器获取（基于客户端IPv6或生成UUID）
  try {
    const response = await fetch('/api/system/user-id', {
      method: 'GET',
      headers: {
        'Content-Type': 'application/json',
      },
    })

    if (response.ok) {
      const data = await response.json()
      userId = data.userId
      
      // 保存到 localStorage
      localStorage.setItem(USER_ID_KEY, userId)
      
      // 保存用户信息
      if (data.userInfo) {
        localStorage.setItem(USER_INFO_KEY, JSON.stringify(data.userInfo))
      }
      
      console.log('✅ 从服务器获取用户ID:', userId)
      return userId
    }
  } catch (error) {
    console.warn('⚠️ 无法从服务器获取用户ID:', error)
  }

  // 3. 本地生成（备用方案）
  userId = generateLocalUserId()
  localStorage.setItem(USER_ID_KEY, userId)
  console.log('🔧 本地生成用户ID:', userId)
  
  return userId
}

/**
 * 获取用户信息
 */
export function getUserInfo() {
  const userInfoStr = localStorage.getItem(USER_INFO_KEY)
  if (userInfoStr) {
    try {
      return JSON.parse(userInfoStr)
    } catch (e) {
      return null
    }
  }
  return null
}

/**
 * 设置用户信息
 */
export function setUserInfo(userInfo) {
  localStorage.setItem(USER_INFO_KEY, JSON.stringify(userInfo))
}

/**
 * 清除用户信息（用于切换用户）
 */
export function clearUserInfo() {
  localStorage.removeItem(USER_ID_KEY)
  localStorage.removeItem(USER_INFO_KEY)
  console.log('🗑️ 已清除用户信息')
}

/**
 * 本地生成用户ID
 * 使用时间戳 + 随机数生成
 */
function generateLocalUserId() {
  const timestamp = Date.now()
  const random = Math.random().toString(36).substring(2, 15)
  return `local_${timestamp}_${random}`
}

/**
 * 初始化用户ID（在应用启动时调用）
 */
export async function initializeUserId() {
  const userId = await getUserId()
  console.log('🎯 用户ID已初始化:', userId)
  return userId
}

export default {
  getUserId,
  getUserInfo,
  setUserInfo,
  clearUserInfo,
  initializeUserId,
}
