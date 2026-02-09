import {
  mkServer,
  listenServer,
  closeServer,
} from "../output/TestServer/index.js";

let fastify;

export async function setup() {
  fastify = mkServer();
  await listenServer(fastify)();
}

export async function teardown() {
  await closeServer(fastify)();
}
