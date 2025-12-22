/**
 * 文档高级搜索组件 (Document Advanced Search Component)
 * 
 * 功能：
 * - 文件名支持正则/包含/等值搜索
 * - 文件类型多选
 * - 时间范围搜索（带校验）
 * - 实时前端过滤
 * - 点击应用调用API
 * - 内联展开式设计，不遮挡文件列表
 *
 * @author AI Reviewer Team
 * @since 2025-12-13
 */

import React, { useState, useCallback, useEffect } from 'react'
import { 
  Card,
  Form, 
  Input, 
  Select, 
  DatePicker, 
  Checkbox, 
  Button, 
  Space, 
  Row, 
  Col,
  message 
} from 'antd'
import { SearchOutlined, ClearOutlined, CloseOutlined } from '@ant-design/icons'
import { useLanguage } from '../../contexts/LanguageContext'
import dayjs from 'dayjs'
import '../../assets/css/document/document-advanced-search.css'

const { RangePicker } = DatePicker

// 文件类型选项
const FILE_TYPES = [
  { label: 'Word文档', value: 'doc,docx', color: '#2B579A' },
  { label: 'Excel表格', value: 'xls,xlsx', color: '#217346' },
  { label: 'PowerPoint', value: 'ppt,pptx', color: '#D24726' },
  { label: 'PDF文档', value: 'pdf', color: '#F40F02' },
  { label: '文本文件', value: 'txt,md', color: '#666666' },
  { label: '图片文件', value: 'jpg,jpeg,png,gif,bmp,svg', color: '#87CEEB' },
  { label: '压缩文件', value: 'zip,rar,7z,tar,gz', color: '#FFA500' },
  { label: '代码文件', value: 'js,jsx,ts,tsx,java,py,cpp,c,html,css,json,xml,yaml,yml', color: '#3776AB' },
]

function DocumentAdvancedSearch(props) {
  const { visible, onClose, onFilter, onApply } = props
  const { t } = useLanguage()
  const [form] = Form.useForm()

  // 表单值变化时触发前端过滤
  const handleValuesChange = useCallback((changedValues, allValues) => {
    // 时间范围校验
    if (allValues.dateRange && allValues.dateRange.length === 2) {
      const [start, end] = allValues.dateRange
      if (start && end && start.isAfter(end)) {
        message.warning('开始时间不能晚于结束时间')
        return
      }
    }

    // 触发前端过滤
    if (onFilter) {
      onFilter(buildFilterCriteria(allValues))
    }
  }, [onFilter])

  // 构建过滤条件
  const buildFilterCriteria = useCallback((values) => {
    return {
      fileNamePattern: values.fileNamePattern || '',
      fileNameMatchType: values.fileNameMatchType || 'contains',
      fileTypes: values.fileTypes || [],
      dateRange: values.dateRange || null,
    }
  }, [])

  // 应用搜索（调用API）
  const handleApply = useCallback(() => {
    form.validateFields().then((values) => {
      // 时间范围校验
      if (values.dateRange && values.dateRange.length === 2) {
        const [start, end] = values.dateRange
        if (start.isAfter(end)) {
          message.error('开始时间不能晚于结束时间')
          return
        }
      }

      if (onApply) {
        onApply(buildFilterCriteria(values))
      }
      message.success('已应用高级搜索条件')
    })
  }, [form, onApply, buildFilterCriteria])

  // 重置表单
  const handleReset = useCallback(() => {
    form.resetFields()
    if (onFilter) {
      onFilter({
        fileNamePattern: '',
        fileNameMatchType: 'contains',
        fileTypes: [],
        dateRange: null,
      })
    }
    message.info('已重置搜索条件')
  }, [form, onFilter])

  if (!visible) return null

  return (
    <Card className="document-advanced-search" styles={{ body: { padding: '20px' } }}>
      <div className="document-advanced-search__header">
        <span className="document-advanced-search__title">🔍 高级搜索</span>
        <Button 
          type="text" 
          icon={<CloseOutlined />} 
          onClick={onClose}
          size="small"
        />
      </div>

      <Form
        form={form}
        layout="inline"
        onValuesChange={handleValuesChange}
        initialValues={{
          fileNameMatchType: 'contains',
          fileTypes: [],
        }}
        className="document-advanced-search__form"
      >
        <Row gutter={[16, 16]} style={{ width: '100%' }}>
          {/* 第一行：文件名搜索 */}
          <Col span={4}>
            <Form.Item
              label="匹配方式"
              name="fileNameMatchType"
              style={{ marginBottom: 0 }}
            >
              <Select size="large" style={{ width: '100%' }}>
                <Select.Option value="contains">包含</Select.Option>
                <Select.Option value="equals">等于</Select.Option>
                <Select.Option value="regex">正则</Select.Option>
              </Select>
            </Form.Item>
          </Col>

          <Col span={8}>
            <Form.Item
              label="文件名"
              name="fileNamePattern"
              style={{ marginBottom: 0 }}
            >
              <Input
                placeholder="输入文件名或正则表达式"
                allowClear
                size="large"
              />
            </Form.Item>
          </Col>

          {/* 时间范围 */}
          <Col span={8}>
            <Form.Item
              label="上传时间"
              name="dateRange"
              style={{ marginBottom: 0 }}
              rules={[
                {
                  validator: (_, value) => {
                    if (!value || value.length !== 2) {
                      return Promise.resolve()
                    }
                    const [start, end] = value
                    if (start && end && start.isAfter(end)) {
                      return Promise.reject(new Error('开始时间不能晚于结束时间'))
                    }
                    return Promise.resolve()
                  }
                }
              ]}
            >
              <RangePicker
                size="large"
                style={{ width: '100%' }}
                format="YYYY-MM-DD"
                placeholder={['开始日期', '结束日期']}
                disabledDate={(current) => current && current > dayjs().endOf('day')}
              />
            </Form.Item>
          </Col>

          {/* 操作按钮 */}
          <Col span={4} style={{ textAlign: 'right' }}>
            <Space>
              <Button onClick={handleReset} icon={<ClearOutlined />} size="large">
                重置
              </Button>
              <Button type="primary" onClick={handleApply} icon={<SearchOutlined />} size="large">
                应用
              </Button>
            </Space>
          </Col>

          {/* 第二行：文件类型 */}
          <Col span={24}>
            <Form.Item
              label="文件类型"
              name="fileTypes"
              style={{ marginBottom: 0 }}
            >
              <Checkbox.Group style={{ width: '100%' }}>
                <Row gutter={[12, 8]}>
                  {FILE_TYPES.map((type) => (
                    <Col span={3} key={type.value}>
                      <Checkbox value={type.value}>
                        <span style={{ color: type.color, fontSize: '13px' }}>
                          {type.label}
                        </span>
                      </Checkbox>
                    </Col>
                  ))}
                </Row>
              </Checkbox.Group>
            </Form.Item>
          </Col>
        </Row>
      </Form>

      <div className="document-advanced-search__tip">
        💡 <strong>提示：</strong>修改条件后自动过滤当前页 • 点击"应用"调用后端API • 正则示例: ^test.*\.pdf$
      </div>
    </Card>
  )
}

export default DocumentAdvancedSearch
