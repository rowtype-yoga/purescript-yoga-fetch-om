import { listen, close } from "./test-server.js";

export async function setup() {
  await listen();
}

export async function teardown() {
  await close();
}
