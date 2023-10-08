module Puppeteer.CDPSession.Event where

import Prelude

import Effect.Uncurried (mkEffectFn1)
import Foreign (Foreign)
import Node.EventEmitter (EventHandle(..))
import Node.EventEmitter.UtilTypes (EventHandle1, EventHandle0)
import Puppeteer.Base (CDPSession)

consoleMessageAddedH :: EventHandle1 CDPSession Foreign
consoleMessageAddedH = EventHandle "Console.messageAdded" mkEffectFn1

debuggerBreakpointResolvedH :: EventHandle1 CDPSession Foreign
debuggerBreakpointResolvedH = EventHandle "Debugger.breakpointResolved" mkEffectFn1

debuggerPausedH :: EventHandle1 CDPSession Foreign
debuggerPausedH = EventHandle "Debugger.paused" mkEffectFn1

debuggerResumedH :: EventHandle0 CDPSession
debuggerResumedH = EventHandle "Debugger.resumed" identity

debuggerScriptFailedToParseH :: EventHandle1 CDPSession Foreign
debuggerScriptFailedToParseH = EventHandle "Debugger.scriptFailedToParse" mkEffectFn1

debuggerScriptParsedH :: EventHandle1 CDPSession Foreign
debuggerScriptParsedH = EventHandle "Debugger.scriptParsed" mkEffectFn1

heapProfilerAddHeapSnapshotChunkH :: EventHandle1 CDPSession Foreign
heapProfilerAddHeapSnapshotChunkH = EventHandle "HeapProfiler.addHeapSnapshotChunk" mkEffectFn1

heapProfilerHeapStatsUpdateH :: EventHandle1 CDPSession Foreign
heapProfilerHeapStatsUpdateH = EventHandle "HeapProfiler.heapStatsUpdate" mkEffectFn1

heapProfilerLastSeenObjectIdH :: EventHandle1 CDPSession Foreign
heapProfilerLastSeenObjectIdH = EventHandle "HeapProfiler.lastSeenObjectId" mkEffectFn1

heapProfilerReportHeapSnapshotProgressH :: EventHandle1 CDPSession Foreign
heapProfilerReportHeapSnapshotProgressH = EventHandle "HeapProfiler.reportHeapSnapshotProgress" mkEffectFn1

heapProfilerResetProfilesH :: EventHandle0 CDPSession
heapProfilerResetProfilesH = EventHandle "HeapProfiler.resetProfiles" identity

profilerConsoleProfileFinishedH :: EventHandle1 CDPSession Foreign
profilerConsoleProfileFinishedH = EventHandle "Profiler.consoleProfileFinished" mkEffectFn1

profilerConsoleProfileStartedH :: EventHandle1 CDPSession Foreign
profilerConsoleProfileStartedH = EventHandle "Profiler.consoleProfileStarted" mkEffectFn1

profilerPreciseCoverageDeltaUpdateH :: EventHandle1 CDPSession Foreign
profilerPreciseCoverageDeltaUpdateH = EventHandle "Profiler.preciseCoverageDeltaUpdate" mkEffectFn1

runtimeBindingCalledH :: EventHandle1 CDPSession Foreign
runtimeBindingCalledH = EventHandle "Runtime.bindingCalled" mkEffectFn1

runtimeConsoleAPICalledH :: EventHandle1 CDPSession Foreign
runtimeConsoleAPICalledH = EventHandle "Runtime.consoleAPICalled" mkEffectFn1

runtimeExceptionRevokedH :: EventHandle1 CDPSession Foreign
runtimeExceptionRevokedH = EventHandle "Runtime.exceptionRevoked" mkEffectFn1

runtimeExceptionThrownH :: EventHandle1 CDPSession Foreign
runtimeExceptionThrownH = EventHandle "Runtime.exceptionThrown" mkEffectFn1

runtimeExecutionContextCreatedH :: EventHandle1 CDPSession Foreign
runtimeExecutionContextCreatedH = EventHandle "Runtime.executionContextCreated" mkEffectFn1

runtimeExecutionContextDestroyedH :: EventHandle1 CDPSession Foreign
runtimeExecutionContextDestroyedH = EventHandle "Runtime.executionContextDestroyed" mkEffectFn1

runtimeExecutionContextsClearedH :: EventHandle0 CDPSession
runtimeExecutionContextsClearedH = EventHandle "Runtime.executionContextsCleared" identity

runtimeInspectRequestedH :: EventHandle1 CDPSession Foreign
runtimeInspectRequestedH = EventHandle "Runtime.inspectRequested" mkEffectFn1

accessibilityLoadCompleteH :: EventHandle1 CDPSession Foreign
accessibilityLoadCompleteH = EventHandle "Accessibility.loadComplete" mkEffectFn1

accessibilityNodesUpdatedH :: EventHandle1 CDPSession Foreign
accessibilityNodesUpdatedH = EventHandle "Accessibility.nodesUpdated" mkEffectFn1

animationAnimationCanceledH :: EventHandle1 CDPSession Foreign
animationAnimationCanceledH = EventHandle "Animation.animationCanceled" mkEffectFn1

animationAnimationCreatedH :: EventHandle1 CDPSession Foreign
animationAnimationCreatedH = EventHandle "Animation.animationCreated" mkEffectFn1

animationAnimationStartedH :: EventHandle1 CDPSession Foreign
animationAnimationStartedH = EventHandle "Animation.animationStarted" mkEffectFn1

auditsIssueAddedH :: EventHandle1 CDPSession Foreign
auditsIssueAddedH = EventHandle "Audits.issueAdded" mkEffectFn1

backgroundServiceRecordingStateChangedH :: EventHandle1 CDPSession Foreign
backgroundServiceRecordingStateChangedH = EventHandle "BackgroundService.recordingStateChanged" mkEffectFn1

backgroundServiceBackgroundServiceEventReceivedH :: EventHandle1 CDPSession Foreign
backgroundServiceBackgroundServiceEventReceivedH = EventHandle "BackgroundService.backgroundServiceEventReceived" mkEffectFn1

browserDownloadWillBeginH :: EventHandle1 CDPSession Foreign
browserDownloadWillBeginH = EventHandle "Browser.downloadWillBegin" mkEffectFn1

browserDownloadProgressH :: EventHandle1 CDPSession Foreign
browserDownloadProgressH = EventHandle "Browser.downloadProgress" mkEffectFn1

cSSFontsUpdatedH :: EventHandle1 CDPSession Foreign
cSSFontsUpdatedH = EventHandle "CSS.fontsUpdated" mkEffectFn1

cSSMediaQueryResultChangedH :: EventHandle0 CDPSession
cSSMediaQueryResultChangedH = EventHandle "CSS.mediaQueryResultChanged" identity

cSSStyleSheetAddedH :: EventHandle1 CDPSession Foreign
cSSStyleSheetAddedH = EventHandle "CSS.styleSheetAdded" mkEffectFn1

cSSStyleSheetChangedH :: EventHandle1 CDPSession Foreign
cSSStyleSheetChangedH = EventHandle "CSS.styleSheetChanged" mkEffectFn1

cSSStyleSheetRemovedH :: EventHandle1 CDPSession Foreign
cSSStyleSheetRemovedH = EventHandle "CSS.styleSheetRemoved" mkEffectFn1

castSinksUpdatedH :: EventHandle1 CDPSession Foreign
castSinksUpdatedH = EventHandle "Cast.sinksUpdated" mkEffectFn1

castIssueUpdatedH :: EventHandle1 CDPSession Foreign
castIssueUpdatedH = EventHandle "Cast.issueUpdated" mkEffectFn1

dOMAttributeModifiedH :: EventHandle1 CDPSession Foreign
dOMAttributeModifiedH = EventHandle "DOM.attributeModified" mkEffectFn1

dOMAttributeRemovedH :: EventHandle1 CDPSession Foreign
dOMAttributeRemovedH = EventHandle "DOM.attributeRemoved" mkEffectFn1

dOMCharacterDataModifiedH :: EventHandle1 CDPSession Foreign
dOMCharacterDataModifiedH = EventHandle "DOM.characterDataModified" mkEffectFn1

dOMChildNodeCountUpdatedH :: EventHandle1 CDPSession Foreign
dOMChildNodeCountUpdatedH = EventHandle "DOM.childNodeCountUpdated" mkEffectFn1

dOMChildNodeInsertedH :: EventHandle1 CDPSession Foreign
dOMChildNodeInsertedH = EventHandle "DOM.childNodeInserted" mkEffectFn1

dOMChildNodeRemovedH :: EventHandle1 CDPSession Foreign
dOMChildNodeRemovedH = EventHandle "DOM.childNodeRemoved" mkEffectFn1

dOMDistributedNodesUpdatedH :: EventHandle1 CDPSession Foreign
dOMDistributedNodesUpdatedH = EventHandle "DOM.distributedNodesUpdated" mkEffectFn1

dOMDocumentUpdatedH :: EventHandle0 CDPSession
dOMDocumentUpdatedH = EventHandle "DOM.documentUpdated" identity

dOMInlineStyleInvalidatedH :: EventHandle1 CDPSession Foreign
dOMInlineStyleInvalidatedH = EventHandle "DOM.inlineStyleInvalidated" mkEffectFn1

dOMPseudoElementAddedH :: EventHandle1 CDPSession Foreign
dOMPseudoElementAddedH = EventHandle "DOM.pseudoElementAdded" mkEffectFn1

dOMTopLayerElementsUpdatedH :: EventHandle0 CDPSession
dOMTopLayerElementsUpdatedH = EventHandle "DOM.topLayerElementsUpdated" identity

dOMPseudoElementRemovedH :: EventHandle1 CDPSession Foreign
dOMPseudoElementRemovedH = EventHandle "DOM.pseudoElementRemoved" mkEffectFn1

dOMSetChildNodesH :: EventHandle1 CDPSession Foreign
dOMSetChildNodesH = EventHandle "DOM.setChildNodes" mkEffectFn1

dOMShadowRootPoppedH :: EventHandle1 CDPSession Foreign
dOMShadowRootPoppedH = EventHandle "DOM.shadowRootPopped" mkEffectFn1

dOMShadowRootPushedH :: EventHandle1 CDPSession Foreign
dOMShadowRootPushedH = EventHandle "DOM.shadowRootPushed" mkEffectFn1

dOMStorageDomStorageItemAddedH :: EventHandle1 CDPSession Foreign
dOMStorageDomStorageItemAddedH = EventHandle "DOMStorage.domStorageItemAdded" mkEffectFn1

dOMStorageDomStorageItemRemovedH :: EventHandle1 CDPSession Foreign
dOMStorageDomStorageItemRemovedH = EventHandle "DOMStorage.domStorageItemRemoved" mkEffectFn1

dOMStorageDomStorageItemUpdatedH :: EventHandle1 CDPSession Foreign
dOMStorageDomStorageItemUpdatedH = EventHandle "DOMStorage.domStorageItemUpdated" mkEffectFn1

dOMStorageDomStorageItemsClearedH :: EventHandle1 CDPSession Foreign
dOMStorageDomStorageItemsClearedH = EventHandle "DOMStorage.domStorageItemsCleared" mkEffectFn1

databaseAddDatabaseH :: EventHandle1 CDPSession Foreign
databaseAddDatabaseH = EventHandle "Database.addDatabase" mkEffectFn1

emulationVirtualTimeBudgetExpiredH :: EventHandle0 CDPSession
emulationVirtualTimeBudgetExpiredH = EventHandle "Emulation.virtualTimeBudgetExpired" identity

inputDragInterceptedH :: EventHandle1 CDPSession Foreign
inputDragInterceptedH = EventHandle "Input.dragIntercepted" mkEffectFn1

inspectorDetachedH :: EventHandle1 CDPSession Foreign
inspectorDetachedH = EventHandle "Inspector.detached" mkEffectFn1

inspectorTargetCrashedH :: EventHandle0 CDPSession
inspectorTargetCrashedH = EventHandle "Inspector.targetCrashed" identity

inspectorTargetReloadedAfterCrashH :: EventHandle0 CDPSession
inspectorTargetReloadedAfterCrashH = EventHandle "Inspector.targetReloadedAfterCrash" identity

layerTreeLayerPaintedH :: EventHandle1 CDPSession Foreign
layerTreeLayerPaintedH = EventHandle "LayerTree.layerPainted" mkEffectFn1

layerTreeLayerTreeDidChangeH :: EventHandle1 CDPSession Foreign
layerTreeLayerTreeDidChangeH = EventHandle "LayerTree.layerTreeDidChange" mkEffectFn1

logEntryAddedH :: EventHandle1 CDPSession Foreign
logEntryAddedH = EventHandle "Log.entryAdded" mkEffectFn1

networkDataReceivedH :: EventHandle1 CDPSession Foreign
networkDataReceivedH = EventHandle "Network.dataReceived" mkEffectFn1

networkEventSourceMessageReceivedH :: EventHandle1 CDPSession Foreign
networkEventSourceMessageReceivedH = EventHandle "Network.eventSourceMessageReceived" mkEffectFn1

networkLoadingFailedH :: EventHandle1 CDPSession Foreign
networkLoadingFailedH = EventHandle "Network.loadingFailed" mkEffectFn1

networkLoadingFinishedH :: EventHandle1 CDPSession Foreign
networkLoadingFinishedH = EventHandle "Network.loadingFinished" mkEffectFn1

networkRequestInterceptedH :: EventHandle1 CDPSession Foreign
networkRequestInterceptedH = EventHandle "Network.requestIntercepted" mkEffectFn1

networkRequestServedFromCacheH :: EventHandle1 CDPSession Foreign
networkRequestServedFromCacheH = EventHandle "Network.requestServedFromCache" mkEffectFn1

networkRequestWillBeSentH :: EventHandle1 CDPSession Foreign
networkRequestWillBeSentH = EventHandle "Network.requestWillBeSent" mkEffectFn1

networkResourceChangedPriorityH :: EventHandle1 CDPSession Foreign
networkResourceChangedPriorityH = EventHandle "Network.resourceChangedPriority" mkEffectFn1

networkSignedExchangeReceivedH :: EventHandle1 CDPSession Foreign
networkSignedExchangeReceivedH = EventHandle "Network.signedExchangeReceived" mkEffectFn1

networkResponseReceivedH :: EventHandle1 CDPSession Foreign
networkResponseReceivedH = EventHandle "Network.responseReceived" mkEffectFn1

networkWebSocketClosedH :: EventHandle1 CDPSession Foreign
networkWebSocketClosedH = EventHandle "Network.webSocketClosed" mkEffectFn1

networkWebSocketCreatedH :: EventHandle1 CDPSession Foreign
networkWebSocketCreatedH = EventHandle "Network.webSocketCreated" mkEffectFn1

networkWebSocketFrameErrorH :: EventHandle1 CDPSession Foreign
networkWebSocketFrameErrorH = EventHandle "Network.webSocketFrameError" mkEffectFn1

networkWebSocketFrameReceivedH :: EventHandle1 CDPSession Foreign
networkWebSocketFrameReceivedH = EventHandle "Network.webSocketFrameReceived" mkEffectFn1

networkWebSocketFrameSentH :: EventHandle1 CDPSession Foreign
networkWebSocketFrameSentH = EventHandle "Network.webSocketFrameSent" mkEffectFn1

networkWebSocketHandshakeResponseReceivedH :: EventHandle1 CDPSession Foreign
networkWebSocketHandshakeResponseReceivedH = EventHandle "Network.webSocketHandshakeResponseReceived" mkEffectFn1

networkWebSocketWillSendHandshakeRequestH :: EventHandle1 CDPSession Foreign
networkWebSocketWillSendHandshakeRequestH = EventHandle "Network.webSocketWillSendHandshakeRequest" mkEffectFn1

networkWebTransportCreatedH :: EventHandle1 CDPSession Foreign
networkWebTransportCreatedH = EventHandle "Network.webTransportCreated" mkEffectFn1

networkWebTransportConnectionEstablishedH :: EventHandle1 CDPSession Foreign
networkWebTransportConnectionEstablishedH = EventHandle "Network.webTransportConnectionEstablished" mkEffectFn1

networkWebTransportClosedH :: EventHandle1 CDPSession Foreign
networkWebTransportClosedH = EventHandle "Network.webTransportClosed" mkEffectFn1

networkRequestWillBeSentExtraInfoH :: EventHandle1 CDPSession Foreign
networkRequestWillBeSentExtraInfoH = EventHandle "Network.requestWillBeSentExtraInfo" mkEffectFn1

networkResponseReceivedExtraInfoH :: EventHandle1 CDPSession Foreign
networkResponseReceivedExtraInfoH = EventHandle "Network.responseReceivedExtraInfo" mkEffectFn1

networkTrustTokenOperationDoneH :: EventHandle1 CDPSession Foreign
networkTrustTokenOperationDoneH = EventHandle "Network.trustTokenOperationDone" mkEffectFn1

networkSubresourceWebBundleMetadataReceivedH :: EventHandle1 CDPSession Foreign
networkSubresourceWebBundleMetadataReceivedH = EventHandle "Network.subresourceWebBundleMetadataReceived" mkEffectFn1

networkSubresourceWebBundleMetadataErrorH :: EventHandle1 CDPSession Foreign
networkSubresourceWebBundleMetadataErrorH = EventHandle "Network.subresourceWebBundleMetadataError" mkEffectFn1

networkSubresourceWebBundleInnerResponseParsedH :: EventHandle1 CDPSession Foreign
networkSubresourceWebBundleInnerResponseParsedH = EventHandle "Network.subresourceWebBundleInnerResponseParsed" mkEffectFn1

networkSubresourceWebBundleInnerResponseErrorH :: EventHandle1 CDPSession Foreign
networkSubresourceWebBundleInnerResponseErrorH = EventHandle "Network.subresourceWebBundleInnerResponseError" mkEffectFn1

networkReportingApiReportAddedH :: EventHandle1 CDPSession Foreign
networkReportingApiReportAddedH = EventHandle "Network.reportingApiReportAdded" mkEffectFn1

networkReportingApiReportUpdatedH :: EventHandle1 CDPSession Foreign
networkReportingApiReportUpdatedH = EventHandle "Network.reportingApiReportUpdated" mkEffectFn1

networkReportingApiEndpointsChangedForOriginH :: EventHandle1 CDPSession Foreign
networkReportingApiEndpointsChangedForOriginH = EventHandle "Network.reportingApiEndpointsChangedForOrigin" mkEffectFn1

overlayInspectNodeRequestedH :: EventHandle1 CDPSession Foreign
overlayInspectNodeRequestedH = EventHandle "Overlay.inspectNodeRequested" mkEffectFn1

overlayNodeHighlightRequestedH :: EventHandle1 CDPSession Foreign
overlayNodeHighlightRequestedH = EventHandle "Overlay.nodeHighlightRequested" mkEffectFn1

overlayScreenshotRequestedH :: EventHandle1 CDPSession Foreign
overlayScreenshotRequestedH = EventHandle "Overlay.screenshotRequested" mkEffectFn1

overlayInspectModeCanceledH :: EventHandle0 CDPSession
overlayInspectModeCanceledH = EventHandle "Overlay.inspectModeCanceled" identity

pageDomContentEventFiredH :: EventHandle1 CDPSession Foreign
pageDomContentEventFiredH = EventHandle "Page.domContentEventFired" mkEffectFn1

pageFileChooserOpenedH :: EventHandle1 CDPSession Foreign
pageFileChooserOpenedH = EventHandle "Page.fileChooserOpened" mkEffectFn1

pageFrameAttachedH :: EventHandle1 CDPSession Foreign
pageFrameAttachedH = EventHandle "Page.frameAttached" mkEffectFn1

pageFrameClearedScheduledNavigationH :: EventHandle1 CDPSession Foreign
pageFrameClearedScheduledNavigationH = EventHandle "Page.frameClearedScheduledNavigation" mkEffectFn1

pageFrameDetachedH :: EventHandle1 CDPSession Foreign
pageFrameDetachedH = EventHandle "Page.frameDetached" mkEffectFn1

pageFrameNavigatedH :: EventHandle1 CDPSession Foreign
pageFrameNavigatedH = EventHandle "Page.frameNavigated" mkEffectFn1

pageDocumentOpenedH :: EventHandle1 CDPSession Foreign
pageDocumentOpenedH = EventHandle "Page.documentOpened" mkEffectFn1

pageFrameResizedH :: EventHandle0 CDPSession
pageFrameResizedH = EventHandle "Page.frameResized" identity

pageFrameRequestedNavigationH :: EventHandle1 CDPSession Foreign
pageFrameRequestedNavigationH = EventHandle "Page.frameRequestedNavigation" mkEffectFn1

pageFrameScheduledNavigationH :: EventHandle1 CDPSession Foreign
pageFrameScheduledNavigationH = EventHandle "Page.frameScheduledNavigation" mkEffectFn1

pageFrameStartedLoadingH :: EventHandle1 CDPSession Foreign
pageFrameStartedLoadingH = EventHandle "Page.frameStartedLoading" mkEffectFn1

pageFrameStoppedLoadingH :: EventHandle1 CDPSession Foreign
pageFrameStoppedLoadingH = EventHandle "Page.frameStoppedLoading" mkEffectFn1

pageDownloadWillBeginH :: EventHandle1 CDPSession Foreign
pageDownloadWillBeginH = EventHandle "Page.downloadWillBegin" mkEffectFn1

pageDownloadProgressH :: EventHandle1 CDPSession Foreign
pageDownloadProgressH = EventHandle "Page.downloadProgress" mkEffectFn1

pageInterstitialHiddenH :: EventHandle0 CDPSession
pageInterstitialHiddenH = EventHandle "Page.interstitialHidden" identity

pageInterstitialShownH :: EventHandle0 CDPSession
pageInterstitialShownH = EventHandle "Page.interstitialShown" identity

pageJavascriptDialogClosedH :: EventHandle1 CDPSession Foreign
pageJavascriptDialogClosedH = EventHandle "Page.javascriptDialogClosed" mkEffectFn1

pageJavascriptDialogOpeningH :: EventHandle1 CDPSession Foreign
pageJavascriptDialogOpeningH = EventHandle "Page.javascriptDialogOpening" mkEffectFn1

pageLifecycleEventH :: EventHandle1 CDPSession Foreign
pageLifecycleEventH = EventHandle "Page.lifecycleEvent" mkEffectFn1

pageBackForwardCacheNotUsedH :: EventHandle1 CDPSession Foreign
pageBackForwardCacheNotUsedH = EventHandle "Page.backForwardCacheNotUsed" mkEffectFn1

pageLoadEventFiredH :: EventHandle1 CDPSession Foreign
pageLoadEventFiredH = EventHandle "Page.loadEventFired" mkEffectFn1

pageNavigatedWithinDocumentH :: EventHandle1 CDPSession Foreign
pageNavigatedWithinDocumentH = EventHandle "Page.navigatedWithinDocument" mkEffectFn1

pageScreencastFrameH :: EventHandle1 CDPSession Foreign
pageScreencastFrameH = EventHandle "Page.screencastFrame" mkEffectFn1

pageScreencastVisibilityChangedH :: EventHandle1 CDPSession Foreign
pageScreencastVisibilityChangedH = EventHandle "Page.screencastVisibilityChanged" mkEffectFn1

pageWindowOpenH :: EventHandle1 CDPSession Foreign
pageWindowOpenH = EventHandle "Page.windowOpen" mkEffectFn1

pageCompilationCacheProducedH :: EventHandle1 CDPSession Foreign
pageCompilationCacheProducedH = EventHandle "Page.compilationCacheProduced" mkEffectFn1

performanceMetricsH :: EventHandle1 CDPSession Foreign
performanceMetricsH = EventHandle "Performance.metrics" mkEffectFn1

performanceTimelineTimelineEventAddedH :: EventHandle1 CDPSession Foreign
performanceTimelineTimelineEventAddedH = EventHandle "PerformanceTimeline.timelineEventAdded" mkEffectFn1

securityCertificateErrorH :: EventHandle1 CDPSession Foreign
securityCertificateErrorH = EventHandle "Security.certificateError" mkEffectFn1

securityVisibleSecurityStateChangedH :: EventHandle1 CDPSession Foreign
securityVisibleSecurityStateChangedH = EventHandle "Security.visibleSecurityStateChanged" mkEffectFn1

securitySecurityStateChangedH :: EventHandle1 CDPSession Foreign
securitySecurityStateChangedH = EventHandle "Security.securityStateChanged" mkEffectFn1

serviceWorkerWorkerErrorReportedH :: EventHandle1 CDPSession Foreign
serviceWorkerWorkerErrorReportedH = EventHandle "ServiceWorker.workerErrorReported" mkEffectFn1

serviceWorkerWorkerRegistrationUpdatedH :: EventHandle1 CDPSession Foreign
serviceWorkerWorkerRegistrationUpdatedH = EventHandle "ServiceWorker.workerRegistrationUpdated" mkEffectFn1

serviceWorkerWorkerVersionUpdatedH :: EventHandle1 CDPSession Foreign
serviceWorkerWorkerVersionUpdatedH = EventHandle "ServiceWorker.workerVersionUpdated" mkEffectFn1

storageCacheStorageContentUpdatedH :: EventHandle1 CDPSession Foreign
storageCacheStorageContentUpdatedH = EventHandle "Storage.cacheStorageContentUpdated" mkEffectFn1

storageCacheStorageListUpdatedH :: EventHandle1 CDPSession Foreign
storageCacheStorageListUpdatedH = EventHandle "Storage.cacheStorageListUpdated" mkEffectFn1

storageIndexedDBContentUpdatedH :: EventHandle1 CDPSession Foreign
storageIndexedDBContentUpdatedH = EventHandle "Storage.indexedDBContentUpdated" mkEffectFn1

storageIndexedDBListUpdatedH :: EventHandle1 CDPSession Foreign
storageIndexedDBListUpdatedH = EventHandle "Storage.indexedDBListUpdated" mkEffectFn1

storageInterestGroupAccessedH :: EventHandle1 CDPSession Foreign
storageInterestGroupAccessedH = EventHandle "Storage.interestGroupAccessed" mkEffectFn1

storageSharedStorageAccessedH :: EventHandle1 CDPSession Foreign
storageSharedStorageAccessedH = EventHandle "Storage.sharedStorageAccessed" mkEffectFn1

storageStorageBucketCreatedOrUpdatedH :: EventHandle1 CDPSession Foreign
storageStorageBucketCreatedOrUpdatedH = EventHandle "Storage.storageBucketCreatedOrUpdated" mkEffectFn1

storageStorageBucketDeletedH :: EventHandle1 CDPSession Foreign
storageStorageBucketDeletedH = EventHandle "Storage.storageBucketDeleted" mkEffectFn1

storageAttributionReportingSourceRegisteredH :: EventHandle1 CDPSession Foreign
storageAttributionReportingSourceRegisteredH = EventHandle "Storage.attributionReportingSourceRegistered" mkEffectFn1

targetAttachedToTargetH :: EventHandle1 CDPSession Foreign
targetAttachedToTargetH = EventHandle "Target.attachedToTarget" mkEffectFn1

targetDetachedFromTargetH :: EventHandle1 CDPSession Foreign
targetDetachedFromTargetH = EventHandle "Target.detachedFromTarget" mkEffectFn1

targetReceivedMessageFromTargetH :: EventHandle1 CDPSession Foreign
targetReceivedMessageFromTargetH = EventHandle "Target.receivedMessageFromTarget" mkEffectFn1

targetTargetCreatedH :: EventHandle1 CDPSession Foreign
targetTargetCreatedH = EventHandle "Target.targetCreated" mkEffectFn1

targetTargetDestroyedH :: EventHandle1 CDPSession Foreign
targetTargetDestroyedH = EventHandle "Target.targetDestroyed" mkEffectFn1

targetTargetCrashedH :: EventHandle1 CDPSession Foreign
targetTargetCrashedH = EventHandle "Target.targetCrashed" mkEffectFn1

targetTargetInfoChangedH :: EventHandle1 CDPSession Foreign
targetTargetInfoChangedH = EventHandle "Target.targetInfoChanged" mkEffectFn1

tetheringAcceptedH :: EventHandle1 CDPSession Foreign
tetheringAcceptedH = EventHandle "Tethering.accepted" mkEffectFn1

tracingBufferUsageH :: EventHandle1 CDPSession Foreign
tracingBufferUsageH = EventHandle "Tracing.bufferUsage" mkEffectFn1

tracingDataCollectedH :: EventHandle1 CDPSession Foreign
tracingDataCollectedH = EventHandle "Tracing.dataCollected" mkEffectFn1

tracingTracingCompleteH :: EventHandle1 CDPSession Foreign
tracingTracingCompleteH = EventHandle "Tracing.tracingComplete" mkEffectFn1

fetchRequestPausedH :: EventHandle1 CDPSession Foreign
fetchRequestPausedH = EventHandle "Fetch.requestPaused" mkEffectFn1

fetchAuthRequiredH :: EventHandle1 CDPSession Foreign
fetchAuthRequiredH = EventHandle "Fetch.authRequired" mkEffectFn1

webAudioContextCreatedH :: EventHandle1 CDPSession Foreign
webAudioContextCreatedH = EventHandle "WebAudio.contextCreated" mkEffectFn1

webAudioContextWillBeDestroyedH :: EventHandle1 CDPSession Foreign
webAudioContextWillBeDestroyedH = EventHandle "WebAudio.contextWillBeDestroyed" mkEffectFn1

webAudioContextChangedH :: EventHandle1 CDPSession Foreign
webAudioContextChangedH = EventHandle "WebAudio.contextChanged" mkEffectFn1

webAudioAudioListenerCreatedH :: EventHandle1 CDPSession Foreign
webAudioAudioListenerCreatedH = EventHandle "WebAudio.audioListenerCreated" mkEffectFn1

webAudioAudioListenerWillBeDestroyedH :: EventHandle1 CDPSession Foreign
webAudioAudioListenerWillBeDestroyedH = EventHandle "WebAudio.audioListenerWillBeDestroyed" mkEffectFn1

webAudioAudioNodeCreatedH :: EventHandle1 CDPSession Foreign
webAudioAudioNodeCreatedH = EventHandle "WebAudio.audioNodeCreated" mkEffectFn1

webAudioAudioNodeWillBeDestroyedH :: EventHandle1 CDPSession Foreign
webAudioAudioNodeWillBeDestroyedH = EventHandle "WebAudio.audioNodeWillBeDestroyed" mkEffectFn1

webAudioAudioParamCreatedH :: EventHandle1 CDPSession Foreign
webAudioAudioParamCreatedH = EventHandle "WebAudio.audioParamCreated" mkEffectFn1

webAudioAudioParamWillBeDestroyedH :: EventHandle1 CDPSession Foreign
webAudioAudioParamWillBeDestroyedH = EventHandle "WebAudio.audioParamWillBeDestroyed" mkEffectFn1

webAudioNodesConnectedH :: EventHandle1 CDPSession Foreign
webAudioNodesConnectedH = EventHandle "WebAudio.nodesConnected" mkEffectFn1

webAudioNodesDisconnectedH :: EventHandle1 CDPSession Foreign
webAudioNodesDisconnectedH = EventHandle "WebAudio.nodesDisconnected" mkEffectFn1

webAudioNodeParamConnectedH :: EventHandle1 CDPSession Foreign
webAudioNodeParamConnectedH = EventHandle "WebAudio.nodeParamConnected" mkEffectFn1

webAudioNodeParamDisconnectedH :: EventHandle1 CDPSession Foreign
webAudioNodeParamDisconnectedH = EventHandle "WebAudio.nodeParamDisconnected" mkEffectFn1

webAuthnCredentialAddedH :: EventHandle1 CDPSession Foreign
webAuthnCredentialAddedH = EventHandle "WebAuthn.credentialAdded" mkEffectFn1

webAuthnCredentialAssertedH :: EventHandle1 CDPSession Foreign
webAuthnCredentialAssertedH = EventHandle "WebAuthn.credentialAsserted" mkEffectFn1

mediaPlayerPropertiesChangedH :: EventHandle1 CDPSession Foreign
mediaPlayerPropertiesChangedH = EventHandle "Media.playerPropertiesChanged" mkEffectFn1

mediaPlayerEventsAddedH :: EventHandle1 CDPSession Foreign
mediaPlayerEventsAddedH = EventHandle "Media.playerEventsAdded" mkEffectFn1

mediaPlayerMessagesLoggedH :: EventHandle1 CDPSession Foreign
mediaPlayerMessagesLoggedH = EventHandle "Media.playerMessagesLogged" mkEffectFn1

mediaPlayerErrorsRaisedH :: EventHandle1 CDPSession Foreign
mediaPlayerErrorsRaisedH = EventHandle "Media.playerErrorsRaised" mkEffectFn1

mediaPlayersCreatedH :: EventHandle1 CDPSession Foreign
mediaPlayersCreatedH = EventHandle "Media.playersCreated" mkEffectFn1

deviceAccessDeviceRequestPromptedH :: EventHandle1 CDPSession Foreign
deviceAccessDeviceRequestPromptedH = EventHandle "DeviceAccess.deviceRequestPrompted" mkEffectFn1

preloadRuleSetUpdatedH :: EventHandle1 CDPSession Foreign
preloadRuleSetUpdatedH = EventHandle "Preload.ruleSetUpdated" mkEffectFn1

preloadRuleSetRemovedH :: EventHandle1 CDPSession Foreign
preloadRuleSetRemovedH = EventHandle "Preload.ruleSetRemoved" mkEffectFn1

preloadPrerenderAttemptCompletedH :: EventHandle1 CDPSession Foreign
preloadPrerenderAttemptCompletedH = EventHandle "Preload.prerenderAttemptCompleted" mkEffectFn1

preloadPreloadEnabledStateUpdatedH :: EventHandle1 CDPSession Foreign
preloadPreloadEnabledStateUpdatedH = EventHandle "Preload.preloadEnabledStateUpdated" mkEffectFn1

preloadPrefetchStatusUpdatedH :: EventHandle1 CDPSession Foreign
preloadPrefetchStatusUpdatedH = EventHandle "Preload.prefetchStatusUpdated" mkEffectFn1

preloadPrerenderStatusUpdatedH :: EventHandle1 CDPSession Foreign
preloadPrerenderStatusUpdatedH = EventHandle "Preload.prerenderStatusUpdated" mkEffectFn1

preloadPreloadingAttemptSourcesUpdatedH :: EventHandle1 CDPSession Foreign
preloadPreloadingAttemptSourcesUpdatedH = EventHandle "Preload.preloadingAttemptSourcesUpdated" mkEffectFn1

fedCmDialogShownH :: EventHandle1 CDPSession Foreign
fedCmDialogShownH = EventHandle "FedCm.dialogShown" mkEffectFn1
