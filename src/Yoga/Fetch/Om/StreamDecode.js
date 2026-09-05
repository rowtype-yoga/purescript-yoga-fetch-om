export const newUtf8Decoder = () => new TextDecoder();

export const decodeUtf8Chunks = (decoder) => (chunks) => () => {
  const decoded = [];
  for (const bytes of chunks) {
    const text = decoder.decode(bytes, { stream: true });
    if (text !== "") decoded.push(text);
  }
  return decoded;
};

export const flushUtf8Decoder = (decoder) => () => decoder.decode();
