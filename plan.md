# Project Plan: Haskell Terminal Sharing Program (like tmate.io)

## Goal

Create a command-line application in Haskell that allows two users to share a terminal session in real-time. One user (the sharer) initiates a session, runs a program, and their terminal output and state are streamed to another user (the viewer) who can see and optionally interact with the sharer's terminal.

## Core Features

*   **Terminal Sharing:** Capture the state (output, cursor position, colors) of the sharer's terminal and transmit it.
*   **Real-time Synchronization:** Keep the viewer's terminal display synchronized with the sharer's.
*   **Input Handling:** Allow the viewer to send keystrokes or commands back to the sharer's terminal process.
*   **Session Management:**
    *   Generate unique session IDs.
    *   Allow a sharer to create a session.
    *   Allow a viewer to join a session using its ID.
*   **Client-Server Architecture:** A central server to facilitate communication between sharers and viewers.

## Architecture

The application will likely follow a client-server model:

1.  **Sharer Client:**
    *   Connects to the server.
    *   Spawns a pseudo-terminal (PTY) and runs a shell or specified command within it.
    *   Reads output and terminal state changes from the PTY.
    *   Sends terminal updates to the server.
    *   Receives input commands from the server (sent by the viewer) and writes them to the PTY's input.
2.  **Viewer Client:**
    *   Connects to the server, specifying a session ID to join.
    *   Receives terminal updates from the server.
    *   Renders the received terminal state on its own terminal.
    *   Captures user input (keystrokes).
    *   Sends user input to the server.
3.  **Server:**
    *   Listens for incoming client connections (sharers and viewers).
    *   Manages active sessions, mapping session IDs to connected sharer and viewer clients.
    *   Routes terminal updates from a sharer to its connected viewers.
    *   Routes input commands from a viewer to the corresponding sharer.
    *   Handles session creation and joining requests.

## Technology Choices

*   **Language:** Haskell
*   **Networking:** Use Haskell libraries for low-level TCP sockets (`network`) or potentially higher-level protocols like WebSockets (`websockets`) for easier server-side implementation and potential web client support in the future.
*   **Terminal Interaction:** Utilize Haskell bindings or FFI for interacting with pseudo-terminals (PTY/TTY) on the underlying operating system (e.g., `System.Posix.Pty`).
*   **Concurrency:** Leverage Haskell's built-in concurrency features (lightweight threads, MVars, Channels) for handling multiple clients and asynchronous I/O.
*   **Serialization:** Choose a format for sending terminal updates and input (e.g., a simple binary format or JSON using libraries like `aeson`).

## High-Level Development Steps

1.  **Phase 1: Basic Client-Server Communication:**
    *   Set up a simple TCP server using `network`.
    *   Implement basic client connection and disconnection handling.
    *   Create simple "hello" messages exchanged between clients and server.
2.  **Phase 2: PTY and Output Capture (Sharer):**
    *   Integrate a PTY library.
    *   Learn how to spawn a shell/command within a PTY.
    *   Read output from the PTY in real-time.
    *   Send captured output data from the sharer client to the server.
3.  **Phase 3: Output Display (Viewer):**
    *   Receive output data on the viewer client.
    *   Write the received data to the viewer's terminal. Address challenges like preserving formatting and cursor position (potentially using libraries like `vty` or ANSI escape codes).
4.  **Phase 4: Input Handling (Viewer -> Server -> Sharer):**
    *   Capture keystrokes from the viewer's terminal.
    *   Send captured input from the viewer client to the server.
    *   Route input from the server to the corresponding sharer client.
    *   Write received input on the sharer client to the PTY's input stream.
5.  **Phase 5: Session Management:**
    *   Implement session creation logic on the server (generate ID, track sharer).
    *   Implement session joining logic (validate ID, connect viewer to sharer).
    *   Add messages for session creation/joining/ending.
6.  **Phase 6: Enhancements & Refinements:**
    *   Implement more sophisticated terminal state synchronization (e.g., handling window resizes, cursor movement, colors).
    *   Add basic security (e.g., simple token/password for joining).
    *   Handle multiple viewers per session.
    *   Improve error handling and robustness.
    *   Build and package the application.

## Potential Challenges

*   **PTY Interaction:** Interacting with PTYs can be complex and platform-dependent.
*   **Terminal Emulation:** Accurately capturing and replaying terminal state (cursor, colors, scrolling, control sequences) is difficult.
*   **Latency:** Ensuring low latency for real-time interaction is crucial.
*   **Cross-Platform Compatibility:** Handling differences in PTY behavior and terminal control sequences across operating systems.

This plan provides a roadmap. We can refine it as we start development and encounter specific technical challenges.