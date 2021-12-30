import { defineConfig } from "vite"
import { VitePWA } from 'vite-plugin-pwa'
import elmPlugin from "vite-plugin-elm"

export default defineConfig({
  plugins: [
    VitePWA({
      registerType: 'autoUpdate',
      manifest: {
        name: "Color tuner",
        short_name: "Color tuner",
        display: "standalone",
        description: "Fine-tune your palette with HSLuv",
        background_color: '#ffffff',
      }
    }),
    elmPlugin({
      // debug: false,
    }),
  ],
})
