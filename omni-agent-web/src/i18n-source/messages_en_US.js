/**
 * English i18n messages
 *
 * @author OmniAgent Team
 * @since 3.0.0
 */
export default {
  // ========== API Response Messages ==========
  api: {
    common: {
      success: 'Operation successful',
      failed: 'Operation failed',
      invalid_parameter: 'Invalid parameter: {0}',
      not_found: 'Resource not found',
      unauthorized: 'Unauthorized',
      forbidden: 'Forbidden'
    },

    // Document
    document: {
      upload: {
        success: 'Document uploaded successfully',
        failed: 'Failed to upload document',
        processing: 'Processing document',
        invalid_format: 'Unsupported file format',
        too_large: 'File size exceeds limit'
      },
      delete: {
        success: 'Document deleted successfully',
        failed: 'Failed to delete document',
        notfound: 'Document not found'
      },
      query: {
        success: 'Query successful',
        notfound: 'No documents found',
        empty: 'Document is empty'
      },
      process: {
        start: 'Start processing document',
        completed: 'Document processing completed',
        failed: 'Document processing failed'
      }
    },

    // RAG
    rag: {
      // Processing stages
      stage: {
        upload: '📄 Document Upload',
        extract: '📝 Text Extraction',
        chunk: '✂️ Smart Chunking',
        vectorize: '🧮 Vectorization',
        index: '💾 Index Storage',
        completed: '✅ Processing Completed'
      },
      // Processing status
      status: {
        running: '🔄 Processing',
        completed: '✅ Completed',
        failed: '❌ Failed'
      },
      // Progress operations
      progress: {
        query: {
          start: '🔍 Querying document processing progress: documentId={0}',
          success: '✅ Query successful'
        },
        notfound: '⚠️ Document processing progress not found: documentId={0}',
        delete: {
          start: '🗑️ Deleting document processing progress: documentId={0}',
          success: '✅ Delete successful: documentId={0}'
        }
      },
      // Processing flow
      flow: {
        document: {
          title: 'Document Processing Flow',
          subtitle: 'Real-time tracking of the complete process from upload to indexing'
        },
        steps: {
          upload: {
            title: 'Document Upload',
            desc: 'Upload document to system',
            success: 'File uploaded successfully',
            error: 'File upload failed: {0}'
          },
          extract: {
            title: 'Text Extraction',
            desc: 'Extract text content from document',
            success: 'Text extraction completed',
            error: 'Text extraction failed: {0}',
            progress: 'Processed: {0}/{1} pages'
          },
          chunk: {
            title: 'Smart Chunking',
            desc: 'Chunking with {0} strategy',
            success: 'Chunking completed, {0} chunks',
            error: 'Chunking failed: {0}',
            progress: 'Chunking progress: {0}%'
          },
          vectorize: {
            title: 'Vectorization',
            desc: 'Vectorizing with {0} model',
            success: 'Vectorization completed',
            error: 'Vectorization failed: {0}',
            progress: 'Vectorized: {0}/{1}'
          },
          index: {
            title: 'Index Storage',
            desc: 'Storing to {0}',
            success: 'Indexing completed, size: {0}',
            error: 'Indexing failed: {0}'
          },
          completed: {
            title: 'Processing Completed',
            desc: 'Document successfully indexed to system'
          }
        },
        actions: {
          view_log: 'View Detailed Log',
          retry: 'Retry Processing',
          export: 'Export Report',
          cancel: 'Cancel Processing'
        },
        stats: {
          file_size: 'File Size: {0}',
          chunks: 'Chunks: {0}',
          vectors: 'Vectors: {0}',
          index_size: 'Index Size: {0}',
          total_time: 'Total Time: {0}'
        }
      },
      // Index management
      index: {
        building: 'Building index',
        completed: 'Index build completed',
        failed: 'Index build failed',
        rebuilding: 'Rebuilding index'
      },
      // Query
      query: {
        success: 'Retrieval successful',
        failed: 'Retrieval failed',
        no_results: 'No results found',
        processing: 'Processing retrieval'
      },
      // Configuration
      config: {
        updated: 'Configuration updated successfully',
        invalid: 'Invalid configuration'
      }
    },

    // Authentication
    auth: {
      login: {
        success: 'Login successful',
        failed: 'Login failed',
        invalid_credentials: 'Invalid username or password',
        locked: 'Account locked'
      },
      logout: {
        success: 'Logout successful'
      },
      token: {
        expired: 'Token expired',
        invalid: 'Invalid token'
      }
    },

    // Knowledge Network
    knowledge: {
      build: {
        start: 'Start building knowledge network',
        completed: 'Knowledge network build completed',
        failed: 'Knowledge network build failed'
      },
      query: {
        success: 'Knowledge query successful',
        notfound: 'No knowledge found'
      }
    }
  },

  // ========== Log Messages ==========
  log: {
    document: {
      processing: {
        start: 'Start processing document: {0}',
        completed: 'Document processing completed: {0}',
        failed: 'Document processing failed: {0}, error: {1}'
      },
      upload: {
        start: 'Start uploading document: {0}',
        success: 'Document uploaded successfully: {0}, ID: {1}',
        failed: 'Document upload failed: {0}, error: {1}'
      },
      delete: {
        start: 'Start deleting document: {0}',
        success: 'Document deleted successfully: {0}',
        failed: 'Document deletion failed: {0}'
      }
    },
    rag: {
      index: {
        start: 'Start building index: {0}',
        progress: 'Index building progress: {0}%',
        completed: 'Index build completed, time: {0}ms',
        failed: 'Index build failed: {0}'
      },
      query: {
        start: 'Start retrieval: keyword={0}',
        results: 'Retrieval completed, found {0} results',
        failed: 'Retrieval failed: {0}'
      }
    },
    knowledge: {
      network: {
        build_start: 'Start building knowledge network: {0}',
        build_completed: 'Knowledge network build completed: {0}',
        extraction: 'Extracting knowledge: {0}'
      }
    },
    system: {
      startup: 'System starting up',
      ready: 'System ready',
      shutdown: 'System shutting down'
    }
  }
}

