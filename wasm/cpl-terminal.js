import { Terminal } from 'https://cdn.jsdelivr.net/npm/xterm@5.3.0/+esm';
import { FitAddon } from 'https://cdn.jsdelivr.net/npm/xterm-addon-fit@0.8.0/+esm';
import { WASI, OpenFile, File, ConsoleStdout } from 'https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/+esm';

/**
 * CPL Terminal Manager
 * Manages xterm.js terminal and provides interface for WASM CPL interpreter
 */
class CPLTerminal {
  constructor() {
    this.term = new Terminal({
      cursorBlink: true,
      fontSize: 14,
      fontFamily: 'Menlo, Monaco, "Courier New", monospace',
      theme: {
        background: '#1e1e1e',
        foreground: '#d4d4d4',
        cursor: '#00ff00',
        cursorAccent: '#1e1e1e',
        selection: 'rgba(255, 255, 255, 0.3)',
        black: '#000000',
        red: '#cd3131',
        green: '#0dbc79',
        yellow: '#e5e510',
        blue: '#2472c8',
        magenta: '#bc3fbc',
        cyan: '#11a8cd',
        white: '#e5e5e5',
        brightBlack: '#666666',
        brightRed: '#f14c4c',
        brightGreen: '#23d18b',
        brightYellow: '#f5f543',
        brightBlue: '#3b8eea',
        brightMagenta: '#d670d6',
        brightCyan: '#29b8db',
        brightWhite: '#e5e5e5'
      },
      allowProposedApi: true
    });

    this.fitAddon = new FitAddon();
    this.term.loadAddon(this.fitAddon);

    this.inputBuffer = '';
    this.resolveInput = null;
    this.isInitialized = false;
  }

  /**
   * Initialize terminal and attach event handlers
   */
  initialize() {
    if (this.isInitialized) return;

    const terminalElement = document.getElementById('terminal');
    if (!terminalElement) {
      throw new Error('Terminal element not found');
    }

    this.term.open(terminalElement);
    this.fitAddon.fit();

    // Handle window resize
    window.addEventListener('resize', () => {
      this.fitAddon.fit();
    });

    // Handle keyboard input
    this.term.onData(data => {
      this.handleInput(data);
    });

    this.isInitialized = true;
  }

  /**
   * Handle keyboard input
   * @param {string} data - Input character(s)
   */
  handleInput(data) {
    const code = data.charCodeAt(0);

    // Enter key
    if (code === 13) {
      if (this.resolveInput) {
        const input = this.inputBuffer;
        this.inputBuffer = '';
        this.term.write('\r\n');
        this.resolveInput(input);
        this.resolveInput = null;
      }
      return;
    }

    // Backspace (127) or Ctrl+H (8)
    if (code === 127 || code === 8) {
      if (this.inputBuffer.length > 0) {
        this.inputBuffer = this.inputBuffer.slice(0, -1);
        // Move cursor back, write space, move cursor back again
        this.term.write('\b \b');
      }
      return;
    }

    // Ctrl+C
    if (code === 3) {
      this.term.write('^C\r\n');
      if (this.resolveInput) {
        this.inputBuffer = '';
        this.resolveInput('');
        this.resolveInput = null;
      }
      return;
    }

    // Ctrl+D (EOF)
    if (code === 4) {
      this.term.write('^D\r\n');
      if (this.resolveInput && this.inputBuffer.length === 0) {
        this.resolveInput('quit');
        this.resolveInput = null;
      }
      return;
    }

    // Printable characters (including space)
    if (code >= 32 && code < 127) {
      this.inputBuffer += data;
      this.term.write(data);
      return;
    }

    // Tab key
    if (code === 9) {
      // For now, just insert spaces (future: implement completion)
      const spaces = '  ';
      this.inputBuffer += spaces;
      this.term.write(spaces);
      return;
    }
  }

  /**
   * Print a line to the terminal
   * @param {string} text - Text to print
   */
  printLine(text) {
    // Convert JavaScript string to lines and print each
    const lines = text.split('\n');
    for (let i = 0; i < lines.length; i++) {
      if (i > 0) this.term.write('\r\n');
      this.term.write(lines[i]);
    }
    this.term.write('\r\n');
  }

  /**
   * Read a line from the terminal (async)
   * @param {string} prompt - Prompt to display
   * @returns {Promise<string>} The input line
   */
  async readLine(prompt) {
    return new Promise(resolve => {
      this.term.write(prompt);
      this.resolveInput = resolve;
    });
  }

  /**
   * Write text without newline
   * @param {string} text - Text to write
   */
  write(text) {
    this.term.write(text);
  }

  /**
   * Clear the terminal
   */
  clear() {
    this.term.clear();
  }
}

// Global terminal instance
const cplTerminal = new CPLTerminal();

// WASM FFI exports - these will be called from Haskell
window.terminal_initialize = () => {
  cplTerminal.initialize();
};

window.terminal_printLine = (jsVal) => {
  // Convert JSVal to string
  const text = String(jsVal);
  cplTerminal.printLine(text);
};

window.terminal_readLine = async (jsVal) => {
  // Convert JSVal to string for prompt
  const prompt = String(jsVal);
  const result = await cplTerminal.readLine(prompt);
  return result;
};

/**
 * Show error message
 */
function showError(message, details) {
  const loadingEl = document.getElementById('loading');
  loadingEl.className = 'error';
  loadingEl.innerHTML = `
    <div class="error-title">Failed to load CPL interpreter</div>
    <div>${message}</div>
    ${details ? `<pre style="margin-top: 1rem; text-align: left;">${details}</pre>` : ''}
  `;
}

/**
 * Initialize and load WASM module
 */
async function initCPL() {
  const loadingEl = document.getElementById('loading');
  const terminalContainer = document.getElementById('terminal-container');

  try {
    loadingEl.textContent = 'Downloading WASM module...';

    // Fetch WASM binary
    const response = await fetch('cpl.wasm');
    if (!response.ok) {
      throw new Error(`Failed to fetch cpl.wasm: ${response.status} ${response.statusText}`);
    }

    loadingEl.textContent = 'Loading WASM module...';

    const wasmBytes = await response.arrayBuffer();

    loadingEl.textContent = 'Initializing CPL interpreter...';

    // Import JSFFI glue code generated by post-link.mjs
    const jsffiModule = await import('./cpl.js');

    // Knot-tying: create empty exports object, fill after instantiation
    const __exports = {};
    const jsffi = jsffiModule.default(__exports);

    // Set up WASI shim for browser
    const fds = [
      new OpenFile(new File([])),                                       // stdin
      ConsoleStdout.lineBuffered(msg => console.log(`[CPL] ${msg}`)),   // stdout
      ConsoleStdout.lineBuffered(msg => console.warn(`[CPL] ${msg}`)),  // stderr
    ];
    const wasi = new WASI([], [], fds);

    // Instantiate WASM with JSFFI + WASI imports
    const { instance } = await WebAssembly.instantiate(wasmBytes, {
      ghc_wasm_jsffi: jsffi,
      wasi_snapshot_preview1: wasi.wasiImport,
    });

    // Complete knot-tying: fill in exports
    Object.assign(__exports, instance.exports);

    // Initialize WASI reactor (sets wasi.inst and calls _initialize)
    wasi.initialize(instance);

    // Hide loading indicator and show terminal
    loadingEl.classList.add('hidden');
    terminalContainer.style.display = 'block';

    // Initialize Haskell RTS and start REPL
    instance.exports.hs_init();
    instance.exports.hs_start();

  } catch (error) {
    console.error('CPL initialization error:', error);
    showError(
      error.message || 'An unknown error occurred',
      error.stack
    );
  }
}

// Start initialization when DOM is ready
if (document.readyState === 'loading') {
  document.addEventListener('DOMContentLoaded', initCPL);
} else {
  initCPL();
}
