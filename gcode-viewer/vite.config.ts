import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [
    react(),
    {
      name: "ignore-dts",
      enforce: "pre",
      resolveId(id) {
        if (id.endsWith(".d.ts")) return id; // claim it
      },
      load(id) {
        if (id.endsWith(".d.ts")) return "export {}"; // empty module
      },
    },
  ],
  server: {
    port: 3000,
    open: false,

    watch: {
      ignored: ["node_modules", "output"],
    },
  },

  // optimizeDeps: {
  //   exclude: ["core"], // or a more specific package/alias
  // },
});
