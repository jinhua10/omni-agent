/**
 * 相似问题推荐组件 (Similar Questions Component)
 *
 * 展示与当前问题相似的历史问题
 * (Displays similar historical questions to the current question)
 *
 * @author AI Reviewer Team
 * @since 2025-12-12
 */

import React from 'react'
import { List, Empty, Tag } from 'antd'
import { QuestionCircleOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import '../../assets/css/qa/similar-questions.css'

function SimilarQuestions(props) {
  const { questions, currentQuestion, onQuestionClick } = props
  const { t } = useLanguage()

  const handleQuestionClick = (question) => {
    if (onQuestionClick) {
      onQuestionClick(question.content || question.question)
    }
  }

  const getSimilarityColor = (similarity) => {
    if (similarity >= 0.8) return 'green'
    if (similarity >= 0.6) return 'blue'
    if (similarity >= 0.4) return 'orange'
    return 'default'
  }

  return (
    <div className="similar-questions">
      <div className="similar-questions__header">
        <QuestionCircleOutlined className="similar-questions__icon" />
        <h3 className="similar-questions__title">{t('qa.similarQuestions.title')}</h3>
      </div>

      <div className="similar-questions__content">
        {questions && questions.length > 0 ? (
          <List
            dataSource={questions}
            renderItem={(question, index) => (
              <div
                key={question.id || index}
                className="similar-questions__item"
                onClick={() => handleQuestionClick(question)}
              >
                <div className="similar-questions__item-header">
                  <span className="similar-questions__item-number">{index + 1}</span>
                  {question.similarity && (
                    <Tag
                      color={getSimilarityColor(question.similarity)}
                      className="similar-questions__item-tag"
                    >
                      {Math.round(question.similarity * 100)}%
                    </Tag>
                  )}
                </div>
                <p className="similar-questions__item-text">
                  {question.content || question.question}
                </p>
                {question.answerPreview && (
                  <p className="similar-questions__item-preview">{question.answerPreview}</p>
                )}
              </div>
            )}
          />
        ) : (
          <Empty
            image={Empty.PRESENTED_IMAGE_SIMPLE}
            description={
              currentQuestion
                ? t('qa.similarQuestions.noResults')
                : t('qa.similarQuestions.askFirst')
            }
            className="similar-questions__empty"
          />
        )}
      </div>
    </div>
  )
}

export default SimilarQuestions

