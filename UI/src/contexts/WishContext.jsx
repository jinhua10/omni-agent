/**
 * WishContext 愿望单状态上下文 (Wish List State Context)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React, { createContext, useContext, useState, useCallback } from 'react'
import PropTypes from 'prop-types'
import api from '@api/modules'

const WishContext = createContext()

export function WishProvider({ children }) {
  const [wishes, setWishes] = useState([])
  const [myWishes, setMyWishes] = useState([])
  const [ranking, setRanking] = useState([])
  const [loading, setLoading] = useState(false)

  const fetchWishes = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.wish.getList(params)
      setWishes(data.list || data)
      return data
    } catch (error) {
      console.error('Failed to fetch wishes:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const submitWish = useCallback(async (wishData) => {
    try {
      setLoading(true)
      const result = await api.wish.submit(wishData)
      setWishes((prev) => [result, ...prev])
      setMyWishes((prev) => [result, ...prev])
      return result
    } catch (error) {
      console.error('Failed to submit wish:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const voteWish = useCallback(async (wishId) => {
    try {
      await api.wish.vote(wishId)
      // 更新愿望列表中的投票数
      const updateVotes = (wishes) =>
        wishes.map((wish) => {
          if (wish.id === wishId) {
            return {
              ...wish,
              voteCount: (wish.voteCount || 0) + 1,
              hasVoted: true,
            }
          }
          return wish
        })

      setWishes(updateVotes)
      setRanking(updateVotes)
    } catch (error) {
      console.error('Failed to vote wish:', error)
      throw error
    }
  }, [])

  const cancelVote = useCallback(async (wishId) => {
    try {
      await api.wish.cancelVote(wishId)
      // 更新愿望列表中的投票数
      const updateVotes = (wishes) =>
        wishes.map((wish) => {
          if (wish.id === wishId) {
            return {
              ...wish,
              voteCount: Math.max((wish.voteCount || 0) - 1, 0),
              hasVoted: false,
            }
          }
          return wish
        })

      setWishes(updateVotes)
      setRanking(updateVotes)
    } catch (error) {
      console.error('Failed to cancel vote:', error)
      throw error
    }
  }, [])

  const fetchRanking = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.wish.getRanking(params)
      setRanking(data.list || data)
      return data
    } catch (error) {
      console.error('Failed to fetch ranking:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const fetchMyWishes = useCallback(async (params) => {
    try {
      setLoading(true)
      const data = await api.wish.getMyWishes(params)
      setMyWishes(data.list || data)
      return data
    } catch (error) {
      console.error('Failed to fetch my wishes:', error)
      throw error
    } finally {
      setLoading(false)
    }
  }, [])

  const value = {
    wishes,
    myWishes,
    ranking,
    loading,
    fetchWishes,
    submitWish,
    voteWish,
    cancelVote,
    fetchRanking,
    fetchMyWishes,
  }

  return (
    <WishContext.Provider value={value}>
      {children}
    </WishContext.Provider>
  )
}

WishProvider.propTypes = {
  children: PropTypes.node.isRequired,
}

export function useWish() {
  const context = useContext(WishContext)
  if (!context) {
    throw new Error('useWish must be used within WishProvider')
  }
  return context
}

export default WishContext

