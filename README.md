# HaskyTerm Share

A terminal sharing program inspired by tmate.io, written in Haskell.

## Synopsis

This project is a command-line application for sharing terminal sessions remotely.  It allows one user (the "sharer") to create a shared terminal session, and other users (the "viewers") to connect and view the sharer's terminal. The viewers can also send input to the sharer's terminal.

## Features

*   **Terminal Sharing:** Enables real-time sharing of a terminal session among multiple users.
*   **Input Sharing** Viewers can send input to the sharer's terminal.
*   **PTY (Pseudo-Terminal) Support:** Uses PTYs for proper terminal emulation.
*   **Simple Command-Line Interface:** Easy to use for both the sharer and viewers.
*   **Written in Haskell:** Leverages Haskell's type safety and concurrency features.

## Architecture

The application consists of two executables:

*   **`terminal-share-server`:**  The server component that manages the shared terminal session. It handles connections from both the sharer and viewers.
*   **`terminal-share-client`:**  The client component that acts as the sharer, creating the PTY and forwarding its output.
*   **`terminal-share-viewer`:** The client component that acts strictly as a viewer, receiving the shared terminal output and sending user input.

The server maintains a shared state (using `MVar`) to track the sharer and viewers. The sharer's terminal output is read from a PTY and forwarded to all connected viewers.  Viewer input is sent to the sharer's PTY.

## Dependencies

*   **base:**  Essential Haskell library. Tested with `^>= 4.17`
*   **network:** For network socket programming.
*   **bytestring:**  For efficient byte string manipulation.
*   **unix:**  For low-level UNIX system calls (PTY, signals, file descriptors).
*   **process:**  For managing external processes.
*   **async:**  For asynchronous programming (concurrent tasks).
*   **posix-pty:**  For PTY-related functions.

## Building

The project uses `cabal` for building.

1.  **Clone the repository:**

    ```bash
    git clone <repository_url>
    cd terminal-share
    ```

2.  **Build the executables:**

    ```bash
    cabal build
    ```

3.  **Install the executables (optional):**

    ```bash
    cabal install
    ```

This will create the `terminal-share-server`, `terminal-share-client` and `terminal-share-viewer` executables. These files will be either in your `dist-newstyle/build` directory (if you haven't installed them) or somewhere in your path (if you have installed them).

## Usage

###  Sharer (Client):

1. Run the server.
    ```bash
    ./dist-newstyle/build/x86_64-linux/ghc-9.4.7/terminal-share-server-0.1.0.0/x/terminal-share-server/build/terminal-share-server/terminal-share-server
    ```

2.  Run the client:

    ```bash
    ./dist-newstyle/build/x86_64-linux/ghc-9.4.7/terminal-share-client-0.1.0.0/x/terminal-share-client/build/terminal-share-client/terminal-share-client
    ```

    This will connect to the server (default is `127.0.0.1:3000`) and start sharing your terminal.

### Viewer:

1. Run the server.
    ```bash
    ./dist-newstyle/build/x86_64-linux/ghc-9.4.7/terminal-share-server-0.1.0.0/x/terminal-share-server/build/terminal-share-server/terminal-share-server
    ```
2. Run the viewer:

    ```bash
    ./dist-newstyle/build/x86_64-linux/ghc-9.4.7/terminal-share-viewer-0.1.0.0/x/terminal-share-viewer/build/terminal-share-viewer/terminal-share-viewer
    ```

    This will connect to the server (default is `127.0.0.1:3000`) and display the shared terminal. Input will be sent as well.

### Important Considerations:

*   Make sure the server is running before starting the client or viewer.
*   The client and viewer connect to the default address `127.0.0.1:3000`.  Currently, there are no command line aguments to change the IP or port.

## Future Enhancements

*   **Configuration:** Allow configurable server address and port.
*   **Authentication:** Add authentication to prevent unauthorized access.
*   **Encryption:** Implement encryption for secure communication.
*   **Error Handling:** Improve error handling and reporting.
*   **Disconnect Handling** More robust disconnect handling for both Sharer and Viewers.
*   **Terminal Resizing:** Implement support for terminal resizing (reporting window size changes).
*   **Command Line Arguments:** Allow specifying the server address and port through command-line arguments.
