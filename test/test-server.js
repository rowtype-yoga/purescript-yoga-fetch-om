import { createServer } from "node:http";

const server = createServer((req, res) => {
  const url = new URL(req.url, "http://localhost:44931");
  const match = url.pathname.match(/^\/users\/(\d+)$/);
  if (match && req.method === "GET") {
    res.writeHead(200, { "Content-Type": "application/json" });
    res.end(
      JSON.stringify({ id: parseInt(match[1]), name: "User " + match[1] }),
    );
    return;
  }
  res.writeHead(404);
  res.end("Not Found");
});

export function listen() {
  return new Promise((resolve) => {
    server.listen(44931, () => resolve());
  });
}

export function close() {
  return new Promise((resolve) => {
    server.close(() => resolve());
  });
}
