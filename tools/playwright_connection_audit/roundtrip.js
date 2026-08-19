async (page) => {
  const state = page.__ena3dConnectionAudit;
  if (!state || state.closed !== state.stableClosed) {
    throw new Error("The transport reset during the six-minute hold.");
  }

  const proof = await state.proveServerRoundTrip();
  if (!state.baselineSessionId || proof.sessionId !== state.baselineSessionId) {
    throw new Error("The session identity changed during the six-minute hold.");
  }
  return { serverRoundTrip: true, roundTrips: proof.roundTrips };
}
