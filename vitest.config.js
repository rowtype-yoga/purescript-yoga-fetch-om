import { defineConfig } from "vitest/config";

export default defineConfig({
  test: {
    include: ["output/Test.Main/index.js"],
    environment: "jsdom",
    globals: true,
    globalSetup: ["test/setup.js"],
  },
});
