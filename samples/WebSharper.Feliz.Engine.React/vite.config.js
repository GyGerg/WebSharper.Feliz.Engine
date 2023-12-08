import fs from 'fs/promises';
import { defineConfig } from 'vite';
import http from 'https';
import react from '@vitejs/plugin-react';

export default defineConfig(() => ({
    plugins: [react()],
    esbuild: {
        loader: "jsx",
        jsxInject: `import React from 'react'`
        // jsxFactory: 'h',
        // jsxFragment: 'Fragment'
    },
    resolve: {
        extensions: ['.js', '.jsx'],
        alias: '.jsx'
    },
    server: {
        proxy: {
            '/IApi': {
                target: 'https://localhost:5001',
                changeOrigin: true,
                secure: false,
                agent: new http.Agent()
            }
        }
    }
}))