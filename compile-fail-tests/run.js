import { mkdtempSync, readdirSync, readFileSync, rmSync } from "node:fs";
import { tmpdir } from "node:os";
import { basename, join } from "node:path";

const cwd = new URL("..", import.meta.url).pathname;
const casesDir = join(cwd, "compile-fail-tests");
const decoder = new TextDecoder();

function run(args, options = {}) {
  const proc = Bun.spawnSync(args, {
    cwd,
    stdout: options.stdout ?? "pipe",
    stderr: options.stderr ?? "pipe",
  });
  return {
    code: proc.exitCode,
    stdout: proc.stdout ? decoder.decode(proc.stdout) : "",
    stderr: proc.stderr ? decoder.decode(proc.stderr) : "",
  };
}

function printResult(label, result) {
  console.error(`\n${label} failed with exit code ${result.code}`);
  if (result.stdout) console.error(result.stdout);
  if (result.stderr) console.error(result.stderr);
}

function expectedSubstring(file) {
  const firstLine = readFileSync(file, "utf8").split("\n", 1)[0];
  return firstLine.startsWith("-- EXPECT: ") ? firstLine.slice("-- EXPECT: ".length) : null;
}

const sourcesResult = run(["bunx", "spago", "sources", "--quiet", "--json"]);
if (sourcesResult.code !== 0) {
  printResult("spago sources", sourcesResult);
  process.exit(1);
}

const excludedSourceGlobs = new Set([
  "test/**/*.purs",
  "../purescript-yoga-fastify-om/src/**/*.purs",
  "../purescript-yoga-fastify-om/local-packages/yoga-fastify/src/**/*.purs",
]);

const librarySources = JSON.parse(sourcesResult.stdout)
  .filter((source) => !excludedSourceGlobs.has(source));

const cases = readdirSync(casesDir)
  .filter((file) => file.endsWith(".purs"))
  .sort()
  .map((file) => join("compile-fail-tests", file));

const libOutput = mkdtempSync(join(tmpdir(), "yoga-fetch-om-lib-"));

try {
  const libResult = run(["bunx", "purs", "compile", ...librarySources, "--output", libOutput], { stdout: "ignore" });
  if (libResult.code !== 0) {
    printResult("library compile", libResult);
    process.exit(1);
  }

  let failed = 0;

  for (const testCase of cases) {
    const failOutput = mkdtempSync(join(tmpdir(), "yoga-fetch-om-compile-fail-"));
    try {
      const result = run([
        "bunx",
        "purs",
        "compile",
        ...librarySources,
        testCase,
        "--output",
        failOutput,
      ]);

      const output = `${result.stdout}\n${result.stderr}`;
      const expected = expectedSubstring(join(cwd, testCase));
      const name = basename(testCase, ".purs");

      if (result.code === 0) {
        console.error(`FAIL ${name} - compiled successfully (should have failed)`);
        failed++;
      } else if (expected && !output.includes(expected)) {
        printResult(`FAIL ${name}`, result);
        console.error(`Expected failure to contain: ${expected}`);
        failed++;
      } else {
        console.log(`PASS ${name}`);
      }
    } finally {
      rmSync(failOutput, { recursive: true, force: true });
    }
  }

  if (failed > 0) {
    process.exit(1);
  }
} finally {
  rmSync(libOutput, { recursive: true, force: true });
}
