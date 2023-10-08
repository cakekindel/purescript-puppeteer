module Puppeteer.CDPSession.Command where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (CDPSession)

foreign import send :: Foreign -> String -> CDPSession -> Effect (Promise Foreign)
foreign import send0 :: String -> CDPSession -> Effect (Promise Foreign)

consoleClearMessages :: CDPSession -> Aff Unit
consoleClearMessages = void <<< Promise.toAffE <<< send0 "Console.clearMessages"

consoleDisable :: CDPSession -> Aff Unit
consoleDisable = void <<< Promise.toAffE <<< send0 "Console.disable"

consoleEnable :: CDPSession -> Aff Unit
consoleEnable = void <<< Promise.toAffE <<< send0 "Console.enable"

debuggerContinueToLocation :: Foreign -> CDPSession -> Aff Unit
debuggerContinueToLocation p = void <<< Promise.toAffE <<< send p "Debugger.continueToLocation"

debuggerDisable :: CDPSession -> Aff Unit
debuggerDisable = void <<< Promise.toAffE <<< send0 "Debugger.disable"

debuggerEnable :: Foreign -> CDPSession -> Aff Foreign
debuggerEnable p = Promise.toAffE <<< send p "Debugger.enable"

debuggerEvaluateOnCallFrame :: Foreign -> CDPSession -> Aff Foreign
debuggerEvaluateOnCallFrame p = Promise.toAffE <<< send p "Debugger.evaluateOnCallFrame"

debuggerGetPossibleBreakpoints :: Foreign -> CDPSession -> Aff Foreign
debuggerGetPossibleBreakpoints p = Promise.toAffE <<< send p "Debugger.getPossibleBreakpoints"

debuggerGetScriptSource :: Foreign -> CDPSession -> Aff Foreign
debuggerGetScriptSource p = Promise.toAffE <<< send p "Debugger.getScriptSource"

debuggerDisassembleWasmModule :: Foreign -> CDPSession -> Aff Foreign
debuggerDisassembleWasmModule p = Promise.toAffE <<< send p "Debugger.disassembleWasmModule"

debuggerNextWasmDisassemblyChunk :: Foreign -> CDPSession -> Aff Foreign
debuggerNextWasmDisassemblyChunk p = Promise.toAffE <<< send p "Debugger.nextWasmDisassemblyChunk"

debuggerGetWasmBytecode :: Foreign -> CDPSession -> Aff Foreign
debuggerGetWasmBytecode p = Promise.toAffE <<< send p "Debugger.getWasmBytecode"

debuggerGetStackTrace :: Foreign -> CDPSession -> Aff Foreign
debuggerGetStackTrace p = Promise.toAffE <<< send p "Debugger.getStackTrace"

debuggerPause :: CDPSession -> Aff Unit
debuggerPause = void <<< Promise.toAffE <<< send0 "Debugger.pause"

debuggerPauseOnAsyncCall :: Foreign -> CDPSession -> Aff Unit
debuggerPauseOnAsyncCall p = void <<< Promise.toAffE <<< send p "Debugger.pauseOnAsyncCall"

debuggerRemoveBreakpoint :: Foreign -> CDPSession -> Aff Unit
debuggerRemoveBreakpoint p = void <<< Promise.toAffE <<< send p "Debugger.removeBreakpoint"

debuggerRestartFrame :: Foreign -> CDPSession -> Aff Foreign
debuggerRestartFrame p = Promise.toAffE <<< send p "Debugger.restartFrame"

debuggerResume :: Foreign -> CDPSession -> Aff Unit
debuggerResume p = void <<< Promise.toAffE <<< send p "Debugger.resume"

debuggerSearchInContent :: Foreign -> CDPSession -> Aff Foreign
debuggerSearchInContent p = Promise.toAffE <<< send p "Debugger.searchInContent"

debuggerSetAsyncCallStackDepth :: Foreign -> CDPSession -> Aff Unit
debuggerSetAsyncCallStackDepth p = void <<< Promise.toAffE <<< send p "Debugger.setAsyncCallStackDepth"

debuggerSetBlackboxPatterns :: Foreign -> CDPSession -> Aff Unit
debuggerSetBlackboxPatterns p = void <<< Promise.toAffE <<< send p "Debugger.setBlackboxPatterns"

debuggerSetBlackboxedRanges :: Foreign -> CDPSession -> Aff Unit
debuggerSetBlackboxedRanges p = void <<< Promise.toAffE <<< send p "Debugger.setBlackboxedRanges"

debuggerSetBreakpoint :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpoint p = Promise.toAffE <<< send p "Debugger.setBreakpoint"

debuggerSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Foreign
debuggerSetInstrumentationBreakpoint p = Promise.toAffE <<< send p "Debugger.setInstrumentationBreakpoint"

debuggerSetBreakpointByUrl :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpointByUrl p = Promise.toAffE <<< send p "Debugger.setBreakpointByUrl"

debuggerSetBreakpointOnFunctionCall :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpointOnFunctionCall p = Promise.toAffE <<< send p "Debugger.setBreakpointOnFunctionCall"

debuggerSetBreakpointsActive :: Foreign -> CDPSession -> Aff Unit
debuggerSetBreakpointsActive p = void <<< Promise.toAffE <<< send p "Debugger.setBreakpointsActive"

debuggerSetPauseOnExceptions :: Foreign -> CDPSession -> Aff Unit
debuggerSetPauseOnExceptions p = void <<< Promise.toAffE <<< send p "Debugger.setPauseOnExceptions"

debuggerSetReturnValue :: Foreign -> CDPSession -> Aff Unit
debuggerSetReturnValue p = void <<< Promise.toAffE <<< send p "Debugger.setReturnValue"

debuggerSetScriptSource :: Foreign -> CDPSession -> Aff Foreign
debuggerSetScriptSource p = Promise.toAffE <<< send p "Debugger.setScriptSource"

debuggerSetSkipAllPauses :: Foreign -> CDPSession -> Aff Unit
debuggerSetSkipAllPauses p = void <<< Promise.toAffE <<< send p "Debugger.setSkipAllPauses"

debuggerSetVariableValue :: Foreign -> CDPSession -> Aff Unit
debuggerSetVariableValue p = void <<< Promise.toAffE <<< send p "Debugger.setVariableValue"

debuggerStepInto :: Foreign -> CDPSession -> Aff Unit
debuggerStepInto p = void <<< Promise.toAffE <<< send p "Debugger.stepInto"

debuggerStepOut :: CDPSession -> Aff Unit
debuggerStepOut = void <<< Promise.toAffE <<< send0 "Debugger.stepOut"

debuggerStepOver :: Foreign -> CDPSession -> Aff Unit
debuggerStepOver p = void <<< Promise.toAffE <<< send p "Debugger.stepOver"

heapProfilerAddInspectedHeapObject :: Foreign -> CDPSession -> Aff Unit
heapProfilerAddInspectedHeapObject p = void <<< Promise.toAffE <<< send p "HeapProfiler.addInspectedHeapObject"

heapProfilerCollectGarbage :: CDPSession -> Aff Unit
heapProfilerCollectGarbage = void <<< Promise.toAffE <<< send0 "HeapProfiler.collectGarbage"

heapProfilerDisable :: CDPSession -> Aff Unit
heapProfilerDisable = void <<< Promise.toAffE <<< send0 "HeapProfiler.disable"

heapProfilerEnable :: CDPSession -> Aff Unit
heapProfilerEnable = void <<< Promise.toAffE <<< send0 "HeapProfiler.enable"

heapProfilerGetHeapObjectId :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetHeapObjectId p = Promise.toAffE <<< send p "HeapProfiler.getHeapObjectId"

heapProfilerGetObjectByHeapObjectId :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetObjectByHeapObjectId p = Promise.toAffE <<< send p "HeapProfiler.getObjectByHeapObjectId"

heapProfilerGetSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetSamplingProfile p = Promise.toAffE <<< send p "HeapProfiler.getSamplingProfile"

heapProfilerStartSampling :: Foreign -> CDPSession -> Aff Unit
heapProfilerStartSampling p = void <<< Promise.toAffE <<< send p "HeapProfiler.startSampling"

heapProfilerStartTrackingHeapObjects :: Foreign -> CDPSession -> Aff Unit
heapProfilerStartTrackingHeapObjects p = void <<< Promise.toAffE <<< send p "HeapProfiler.startTrackingHeapObjects"

heapProfilerStopSampling :: Foreign -> CDPSession -> Aff Foreign
heapProfilerStopSampling p = Promise.toAffE <<< send p "HeapProfiler.stopSampling"

heapProfilerStopTrackingHeapObjects :: Foreign -> CDPSession -> Aff Unit
heapProfilerStopTrackingHeapObjects p = void <<< Promise.toAffE <<< send p "HeapProfiler.stopTrackingHeapObjects"

heapProfilerTakeHeapSnapshot :: Foreign -> CDPSession -> Aff Unit
heapProfilerTakeHeapSnapshot p = void <<< Promise.toAffE <<< send p "HeapProfiler.takeHeapSnapshot"

profilerDisable :: CDPSession -> Aff Unit
profilerDisable = void <<< Promise.toAffE <<< send0 "Profiler.disable"

profilerEnable :: CDPSession -> Aff Unit
profilerEnable = void <<< Promise.toAffE <<< send0 "Profiler.enable"

profilerGetBestEffortCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerGetBestEffortCoverage p = Promise.toAffE <<< send p "Profiler.getBestEffortCoverage"

profilerSetSamplingInterval :: Foreign -> CDPSession -> Aff Unit
profilerSetSamplingInterval p = void <<< Promise.toAffE <<< send p "Profiler.setSamplingInterval"

profilerStart :: CDPSession -> Aff Unit
profilerStart = void <<< Promise.toAffE <<< send0 "Profiler.start"

profilerStartPreciseCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerStartPreciseCoverage p = Promise.toAffE <<< send p "Profiler.startPreciseCoverage"

profilerStop :: Foreign -> CDPSession -> Aff Foreign
profilerStop p = Promise.toAffE <<< send p "Profiler.stop"

profilerStopPreciseCoverage :: CDPSession -> Aff Unit
profilerStopPreciseCoverage = void <<< Promise.toAffE <<< send0 "Profiler.stopPreciseCoverage"

profilerTakePreciseCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerTakePreciseCoverage p = Promise.toAffE <<< send p "Profiler.takePreciseCoverage"

runtimeAwaitPromise :: Foreign -> CDPSession -> Aff Foreign
runtimeAwaitPromise p = Promise.toAffE <<< send p "Runtime.awaitPromise"

runtimeCallFunctionOn :: Foreign -> CDPSession -> Aff Foreign
runtimeCallFunctionOn p = Promise.toAffE <<< send p "Runtime.callFunctionOn"

runtimeCompileScript :: Foreign -> CDPSession -> Aff Foreign
runtimeCompileScript p = Promise.toAffE <<< send p "Runtime.compileScript"

runtimeDisable :: CDPSession -> Aff Unit
runtimeDisable = void <<< Promise.toAffE <<< send0 "Runtime.disable"

runtimeDiscardConsoleEntries :: CDPSession -> Aff Unit
runtimeDiscardConsoleEntries = void <<< Promise.toAffE <<< send0 "Runtime.discardConsoleEntries"

runtimeEnable :: CDPSession -> Aff Unit
runtimeEnable = void <<< Promise.toAffE <<< send0 "Runtime.enable"

runtimeEvaluate :: Foreign -> CDPSession -> Aff Foreign
runtimeEvaluate p = Promise.toAffE <<< send p "Runtime.evaluate"

runtimeGetIsolateId :: Foreign -> CDPSession -> Aff Foreign
runtimeGetIsolateId p = Promise.toAffE <<< send p "Runtime.getIsolateId"

runtimeGetHeapUsage :: Foreign -> CDPSession -> Aff Foreign
runtimeGetHeapUsage p = Promise.toAffE <<< send p "Runtime.getHeapUsage"

runtimeGetProperties :: Foreign -> CDPSession -> Aff Foreign
runtimeGetProperties p = Promise.toAffE <<< send p "Runtime.getProperties"

runtimeGlobalLexicalScopeNames :: Foreign -> CDPSession -> Aff Foreign
runtimeGlobalLexicalScopeNames p = Promise.toAffE <<< send p "Runtime.globalLexicalScopeNames"

runtimeQueryObjects :: Foreign -> CDPSession -> Aff Foreign
runtimeQueryObjects p = Promise.toAffE <<< send p "Runtime.queryObjects"

runtimeReleaseObject :: Foreign -> CDPSession -> Aff Unit
runtimeReleaseObject p = void <<< Promise.toAffE <<< send p "Runtime.releaseObject"

runtimeReleaseObjectGroup :: Foreign -> CDPSession -> Aff Unit
runtimeReleaseObjectGroup p = void <<< Promise.toAffE <<< send p "Runtime.releaseObjectGroup"

runtimeRunIfWaitingForDebugger :: CDPSession -> Aff Unit
runtimeRunIfWaitingForDebugger = void <<< Promise.toAffE <<< send0 "Runtime.runIfWaitingForDebugger"

runtimeRunScript :: Foreign -> CDPSession -> Aff Foreign
runtimeRunScript p = Promise.toAffE <<< send p "Runtime.runScript"

runtimeSetAsyncCallStackDepth :: Foreign -> CDPSession -> Aff Unit
runtimeSetAsyncCallStackDepth p = void <<< Promise.toAffE <<< send p "Runtime.setAsyncCallStackDepth"

runtimeSetCustomObjectFormatterEnabled :: Foreign -> CDPSession -> Aff Unit
runtimeSetCustomObjectFormatterEnabled p = void <<< Promise.toAffE <<< send p "Runtime.setCustomObjectFormatterEnabled"

runtimeSetMaxCallStackSizeToCapture :: Foreign -> CDPSession -> Aff Unit
runtimeSetMaxCallStackSizeToCapture p = void <<< Promise.toAffE <<< send p "Runtime.setMaxCallStackSizeToCapture"

runtimeTerminateExecution :: CDPSession -> Aff Unit
runtimeTerminateExecution = void <<< Promise.toAffE <<< send0 "Runtime.terminateExecution"

runtimeAddBinding :: Foreign -> CDPSession -> Aff Unit
runtimeAddBinding p = void <<< Promise.toAffE <<< send p "Runtime.addBinding"

runtimeRemoveBinding :: Foreign -> CDPSession -> Aff Unit
runtimeRemoveBinding p = void <<< Promise.toAffE <<< send p "Runtime.removeBinding"

runtimeGetExceptionDetails :: Foreign -> CDPSession -> Aff Foreign
runtimeGetExceptionDetails p = Promise.toAffE <<< send p "Runtime.getExceptionDetails"

schemaGetDomains :: Foreign -> CDPSession -> Aff Foreign
schemaGetDomains p = Promise.toAffE <<< send p "Schema.getDomains"

accessibilityDisable :: CDPSession -> Aff Unit
accessibilityDisable = void <<< Promise.toAffE <<< send0 "Accessibility.disable"

accessibilityEnable :: CDPSession -> Aff Unit
accessibilityEnable = void <<< Promise.toAffE <<< send0 "Accessibility.enable"

accessibilityGetPartialAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetPartialAXTree p = Promise.toAffE <<< send p "Accessibility.getPartialAXTree"

accessibilityGetFullAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetFullAXTree p = Promise.toAffE <<< send p "Accessibility.getFullAXTree"

accessibilityGetRootAXNode :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetRootAXNode p = Promise.toAffE <<< send p "Accessibility.getRootAXNode"

accessibilityGetAXNodeAndAncestors :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetAXNodeAndAncestors p = Promise.toAffE <<< send p "Accessibility.getAXNodeAndAncestors"

accessibilityGetChildAXNodes :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetChildAXNodes p = Promise.toAffE <<< send p "Accessibility.getChildAXNodes"

accessibilityQueryAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityQueryAXTree p = Promise.toAffE <<< send p "Accessibility.queryAXTree"

animationDisable :: CDPSession -> Aff Unit
animationDisable = void <<< Promise.toAffE <<< send0 "Animation.disable"

animationEnable :: CDPSession -> Aff Unit
animationEnable = void <<< Promise.toAffE <<< send0 "Animation.enable"

animationGetCurrentTime :: Foreign -> CDPSession -> Aff Foreign
animationGetCurrentTime p = Promise.toAffE <<< send p "Animation.getCurrentTime"

animationGetPlaybackRate :: Foreign -> CDPSession -> Aff Foreign
animationGetPlaybackRate p = Promise.toAffE <<< send p "Animation.getPlaybackRate"

animationReleaseAnimations :: Foreign -> CDPSession -> Aff Unit
animationReleaseAnimations p = void <<< Promise.toAffE <<< send p "Animation.releaseAnimations"

animationResolveAnimation :: Foreign -> CDPSession -> Aff Foreign
animationResolveAnimation p = Promise.toAffE <<< send p "Animation.resolveAnimation"

animationSeekAnimations :: Foreign -> CDPSession -> Aff Unit
animationSeekAnimations p = void <<< Promise.toAffE <<< send p "Animation.seekAnimations"

animationSetPaused :: Foreign -> CDPSession -> Aff Unit
animationSetPaused p = void <<< Promise.toAffE <<< send p "Animation.setPaused"

animationSetPlaybackRate :: Foreign -> CDPSession -> Aff Unit
animationSetPlaybackRate p = void <<< Promise.toAffE <<< send p "Animation.setPlaybackRate"

animationSetTiming :: Foreign -> CDPSession -> Aff Unit
animationSetTiming p = void <<< Promise.toAffE <<< send p "Animation.setTiming"

auditsGetEncodedResponse :: Foreign -> CDPSession -> Aff Foreign
auditsGetEncodedResponse p = Promise.toAffE <<< send p "Audits.getEncodedResponse"

auditsDisable :: CDPSession -> Aff Unit
auditsDisable = void <<< Promise.toAffE <<< send0 "Audits.disable"

auditsEnable :: CDPSession -> Aff Unit
auditsEnable = void <<< Promise.toAffE <<< send0 "Audits.enable"

auditsCheckContrast :: Foreign -> CDPSession -> Aff Unit
auditsCheckContrast p = void <<< Promise.toAffE <<< send p "Audits.checkContrast"

auditsCheckFormsIssues :: Foreign -> CDPSession -> Aff Foreign
auditsCheckFormsIssues p = Promise.toAffE <<< send p "Audits.checkFormsIssues"

autofillTrigger :: Foreign -> CDPSession -> Aff Unit
autofillTrigger p = void <<< Promise.toAffE <<< send p "Autofill.trigger"

autofillSetAddresses :: Foreign -> CDPSession -> Aff Unit
autofillSetAddresses p = void <<< Promise.toAffE <<< send p "Autofill.setAddresses"

backgroundServiceStartObserving :: Foreign -> CDPSession -> Aff Unit
backgroundServiceStartObserving p = void <<< Promise.toAffE <<< send p "BackgroundService.startObserving"

backgroundServiceStopObserving :: Foreign -> CDPSession -> Aff Unit
backgroundServiceStopObserving p = void <<< Promise.toAffE <<< send p "BackgroundService.stopObserving"

backgroundServiceSetRecording :: Foreign -> CDPSession -> Aff Unit
backgroundServiceSetRecording p = void <<< Promise.toAffE <<< send p "BackgroundService.setRecording"

backgroundServiceClearEvents :: Foreign -> CDPSession -> Aff Unit
backgroundServiceClearEvents p = void <<< Promise.toAffE <<< send p "BackgroundService.clearEvents"

browserSetPermission :: Foreign -> CDPSession -> Aff Unit
browserSetPermission p = void <<< Promise.toAffE <<< send p "Browser.setPermission"

browserGrantPermissions :: Foreign -> CDPSession -> Aff Unit
browserGrantPermissions p = void <<< Promise.toAffE <<< send p "Browser.grantPermissions"

browserResetPermissions :: Foreign -> CDPSession -> Aff Unit
browserResetPermissions p = void <<< Promise.toAffE <<< send p "Browser.resetPermissions"

browserSetDownloadBehavior :: Foreign -> CDPSession -> Aff Unit
browserSetDownloadBehavior p = void <<< Promise.toAffE <<< send p "Browser.setDownloadBehavior"

browserCancelDownload :: Foreign -> CDPSession -> Aff Unit
browserCancelDownload p = void <<< Promise.toAffE <<< send p "Browser.cancelDownload"

browserClose :: CDPSession -> Aff Unit
browserClose = void <<< Promise.toAffE <<< send0 "Browser.close"

browserCrash :: CDPSession -> Aff Unit
browserCrash = void <<< Promise.toAffE <<< send0 "Browser.crash"

browserCrashGpuProcess :: CDPSession -> Aff Unit
browserCrashGpuProcess = void <<< Promise.toAffE <<< send0 "Browser.crashGpuProcess"

browserGetVersion :: Foreign -> CDPSession -> Aff Foreign
browserGetVersion p = Promise.toAffE <<< send p "Browser.getVersion"

browserGetBrowserCommandLine :: Foreign -> CDPSession -> Aff Foreign
browserGetBrowserCommandLine p = Promise.toAffE <<< send p "Browser.getBrowserCommandLine"

browserGetHistograms :: Foreign -> CDPSession -> Aff Foreign
browserGetHistograms p = Promise.toAffE <<< send p "Browser.getHistograms"

browserGetHistogram :: Foreign -> CDPSession -> Aff Foreign
browserGetHistogram p = Promise.toAffE <<< send p "Browser.getHistogram"

browserGetWindowBounds :: Foreign -> CDPSession -> Aff Foreign
browserGetWindowBounds p = Promise.toAffE <<< send p "Browser.getWindowBounds"

browserGetWindowForTarget :: Foreign -> CDPSession -> Aff Foreign
browserGetWindowForTarget p = Promise.toAffE <<< send p "Browser.getWindowForTarget"

browserSetWindowBounds :: Foreign -> CDPSession -> Aff Unit
browserSetWindowBounds p = void <<< Promise.toAffE <<< send p "Browser.setWindowBounds"

browserSetDockTile :: Foreign -> CDPSession -> Aff Unit
browserSetDockTile p = void <<< Promise.toAffE <<< send p "Browser.setDockTile"

browserExecuteBrowserCommand :: Foreign -> CDPSession -> Aff Unit
browserExecuteBrowserCommand p = void <<< Promise.toAffE <<< send p "Browser.executeBrowserCommand"

browserAddPrivacySandboxEnrollmentOverride :: Foreign -> CDPSession -> Aff Unit
browserAddPrivacySandboxEnrollmentOverride p = void <<< Promise.toAffE <<< send p "Browser.addPrivacySandboxEnrollmentOverride"

cSSAddRule :: Foreign -> CDPSession -> Aff Foreign
cSSAddRule p = Promise.toAffE <<< send p "CSS.addRule"

cSSCollectClassNames :: Foreign -> CDPSession -> Aff Foreign
cSSCollectClassNames p = Promise.toAffE <<< send p "CSS.collectClassNames"

cSSCreateStyleSheet :: Foreign -> CDPSession -> Aff Foreign
cSSCreateStyleSheet p = Promise.toAffE <<< send p "CSS.createStyleSheet"

cSSDisable :: CDPSession -> Aff Unit
cSSDisable = void <<< Promise.toAffE <<< send0 "CSS.disable"

cSSEnable :: CDPSession -> Aff Unit
cSSEnable = void <<< Promise.toAffE <<< send0 "CSS.enable"

cSSForcePseudoState :: Foreign -> CDPSession -> Aff Unit
cSSForcePseudoState p = void <<< Promise.toAffE <<< send p "CSS.forcePseudoState"

cSSGetBackgroundColors :: Foreign -> CDPSession -> Aff Foreign
cSSGetBackgroundColors p = Promise.toAffE <<< send p "CSS.getBackgroundColors"

cSSGetComputedStyleForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetComputedStyleForNode p = Promise.toAffE <<< send p "CSS.getComputedStyleForNode"

cSSGetInlineStylesForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetInlineStylesForNode p = Promise.toAffE <<< send p "CSS.getInlineStylesForNode"

cSSGetMatchedStylesForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetMatchedStylesForNode p = Promise.toAffE <<< send p "CSS.getMatchedStylesForNode"

cSSGetMediaQueries :: Foreign -> CDPSession -> Aff Foreign
cSSGetMediaQueries p = Promise.toAffE <<< send p "CSS.getMediaQueries"

cSSGetPlatformFontsForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetPlatformFontsForNode p = Promise.toAffE <<< send p "CSS.getPlatformFontsForNode"

cSSGetStyleSheetText :: Foreign -> CDPSession -> Aff Foreign
cSSGetStyleSheetText p = Promise.toAffE <<< send p "CSS.getStyleSheetText"

cSSGetLayersForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetLayersForNode p = Promise.toAffE <<< send p "CSS.getLayersForNode"

cSSTrackComputedStyleUpdates :: Foreign -> CDPSession -> Aff Unit
cSSTrackComputedStyleUpdates p = void <<< Promise.toAffE <<< send p "CSS.trackComputedStyleUpdates"

cSSTakeComputedStyleUpdates :: Foreign -> CDPSession -> Aff Foreign
cSSTakeComputedStyleUpdates p = Promise.toAffE <<< send p "CSS.takeComputedStyleUpdates"

cSSSetEffectivePropertyValueForNode :: Foreign -> CDPSession -> Aff Unit
cSSSetEffectivePropertyValueForNode p = void <<< Promise.toAffE <<< send p "CSS.setEffectivePropertyValueForNode"

cSSSetKeyframeKey :: Foreign -> CDPSession -> Aff Foreign
cSSSetKeyframeKey p = Promise.toAffE <<< send p "CSS.setKeyframeKey"

cSSSetMediaText :: Foreign -> CDPSession -> Aff Foreign
cSSSetMediaText p = Promise.toAffE <<< send p "CSS.setMediaText"

cSSSetContainerQueryText :: Foreign -> CDPSession -> Aff Foreign
cSSSetContainerQueryText p = Promise.toAffE <<< send p "CSS.setContainerQueryText"

cSSSetSupportsText :: Foreign -> CDPSession -> Aff Foreign
cSSSetSupportsText p = Promise.toAffE <<< send p "CSS.setSupportsText"

cSSSetScopeText :: Foreign -> CDPSession -> Aff Foreign
cSSSetScopeText p = Promise.toAffE <<< send p "CSS.setScopeText"

cSSSetRuleSelector :: Foreign -> CDPSession -> Aff Foreign
cSSSetRuleSelector p = Promise.toAffE <<< send p "CSS.setRuleSelector"

cSSSetStyleSheetText :: Foreign -> CDPSession -> Aff Foreign
cSSSetStyleSheetText p = Promise.toAffE <<< send p "CSS.setStyleSheetText"

cSSSetStyleTexts :: Foreign -> CDPSession -> Aff Foreign
cSSSetStyleTexts p = Promise.toAffE <<< send p "CSS.setStyleTexts"

cSSStartRuleUsageTracking :: CDPSession -> Aff Unit
cSSStartRuleUsageTracking = void <<< Promise.toAffE <<< send0 "CSS.startRuleUsageTracking"

cSSStopRuleUsageTracking :: Foreign -> CDPSession -> Aff Foreign
cSSStopRuleUsageTracking p = Promise.toAffE <<< send p "CSS.stopRuleUsageTracking"

cSSTakeCoverageDelta :: Foreign -> CDPSession -> Aff Foreign
cSSTakeCoverageDelta p = Promise.toAffE <<< send p "CSS.takeCoverageDelta"

cSSSetLocalFontsEnabled :: Foreign -> CDPSession -> Aff Unit
cSSSetLocalFontsEnabled p = void <<< Promise.toAffE <<< send p "CSS.setLocalFontsEnabled"

cacheStorageDeleteCache :: Foreign -> CDPSession -> Aff Unit
cacheStorageDeleteCache p = void <<< Promise.toAffE <<< send p "CacheStorage.deleteCache"

cacheStorageDeleteEntry :: Foreign -> CDPSession -> Aff Unit
cacheStorageDeleteEntry p = void <<< Promise.toAffE <<< send p "CacheStorage.deleteEntry"

cacheStorageRequestCacheNames :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestCacheNames p = Promise.toAffE <<< send p "CacheStorage.requestCacheNames"

cacheStorageRequestCachedResponse :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestCachedResponse p = Promise.toAffE <<< send p "CacheStorage.requestCachedResponse"

cacheStorageRequestEntries :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestEntries p = Promise.toAffE <<< send p "CacheStorage.requestEntries"

castEnable :: Foreign -> CDPSession -> Aff Unit
castEnable p = void <<< Promise.toAffE <<< send p "Cast.enable"

castDisable :: CDPSession -> Aff Unit
castDisable = void <<< Promise.toAffE <<< send0 "Cast.disable"

castSetSinkToUse :: Foreign -> CDPSession -> Aff Unit
castSetSinkToUse p = void <<< Promise.toAffE <<< send p "Cast.setSinkToUse"

castStartDesktopMirroring :: Foreign -> CDPSession -> Aff Unit
castStartDesktopMirroring p = void <<< Promise.toAffE <<< send p "Cast.startDesktopMirroring"

castStartTabMirroring :: Foreign -> CDPSession -> Aff Unit
castStartTabMirroring p = void <<< Promise.toAffE <<< send p "Cast.startTabMirroring"

castStopCasting :: Foreign -> CDPSession -> Aff Unit
castStopCasting p = void <<< Promise.toAffE <<< send p "Cast.stopCasting"

domCollectClassNamesFromSubtree :: Foreign -> CDPSession -> Aff Foreign
domCollectClassNamesFromSubtree p = Promise.toAffE <<< send p "DOM.collectClassNamesFromSubtree"

domCopyTo :: Foreign -> CDPSession -> Aff Foreign
domCopyTo p = Promise.toAffE <<< send p "DOM.copyTo"

domDescribeNode :: Foreign -> CDPSession -> Aff Foreign
domDescribeNode p = Promise.toAffE <<< send p "DOM.describeNode"

domScrollIntoViewIfNeeded :: Foreign -> CDPSession -> Aff Unit
domScrollIntoViewIfNeeded p = void <<< Promise.toAffE <<< send p "DOM.scrollIntoViewIfNeeded"

domDisable :: CDPSession -> Aff Unit
domDisable = void <<< Promise.toAffE <<< send0 "DOM.disable"

domDiscardSearchResults :: Foreign -> CDPSession -> Aff Unit
domDiscardSearchResults p = void <<< Promise.toAffE <<< send p "DOM.discardSearchResults"

domEnable :: Foreign -> CDPSession -> Aff Unit
domEnable p = void <<< Promise.toAffE <<< send p "DOM.enable"

domFocus :: Foreign -> CDPSession -> Aff Unit
domFocus p = void <<< Promise.toAffE <<< send p "DOM.focus"

domGetAttributes :: Foreign -> CDPSession -> Aff Foreign
domGetAttributes p = Promise.toAffE <<< send p "DOM.getAttributes"

domGetBoxModel :: Foreign -> CDPSession -> Aff Foreign
domGetBoxModel p = Promise.toAffE <<< send p "DOM.getBoxModel"

domGetContentQuads :: Foreign -> CDPSession -> Aff Foreign
domGetContentQuads p = Promise.toAffE <<< send p "DOM.getContentQuads"

domGetDocument :: Foreign -> CDPSession -> Aff Foreign
domGetDocument p = Promise.toAffE <<< send p "DOM.getDocument"

domGetFlattenedDocument :: Foreign -> CDPSession -> Aff Foreign
domGetFlattenedDocument p = Promise.toAffE <<< send p "DOM.getFlattenedDocument"

domGetNodesForSubtreeByStyle :: Foreign -> CDPSession -> Aff Foreign
domGetNodesForSubtreeByStyle p = Promise.toAffE <<< send p "DOM.getNodesForSubtreeByStyle"

domGetNodeForLocation :: Foreign -> CDPSession -> Aff Foreign
domGetNodeForLocation p = Promise.toAffE <<< send p "DOM.getNodeForLocation"

domGetOuterHTML :: Foreign -> CDPSession -> Aff Foreign
domGetOuterHTML p = Promise.toAffE <<< send p "DOM.getOuterHTML"

domGetRelayoutBoundary :: Foreign -> CDPSession -> Aff Foreign
domGetRelayoutBoundary p = Promise.toAffE <<< send p "DOM.getRelayoutBoundary"

domGetSearchResults :: Foreign -> CDPSession -> Aff Foreign
domGetSearchResults p = Promise.toAffE <<< send p "DOM.getSearchResults"

domHideHighlight :: CDPSession -> Aff Unit
domHideHighlight = void <<< Promise.toAffE <<< send0 "DOM.hideHighlight"

domHighlightNode :: CDPSession -> Aff Unit
domHighlightNode = void <<< Promise.toAffE <<< send0 "DOM.highlightNode"

domHighlightRect :: CDPSession -> Aff Unit
domHighlightRect = void <<< Promise.toAffE <<< send0 "DOM.highlightRect"

domMarkUndoableState :: CDPSession -> Aff Unit
domMarkUndoableState = void <<< Promise.toAffE <<< send0 "DOM.markUndoableState"

domMoveTo :: Foreign -> CDPSession -> Aff Foreign
domMoveTo p = Promise.toAffE <<< send p "DOM.moveTo"

domPerformSearch :: Foreign -> CDPSession -> Aff Foreign
domPerformSearch p = Promise.toAffE <<< send p "DOM.performSearch"

domPushNodeByPathToFrontend :: Foreign -> CDPSession -> Aff Foreign
domPushNodeByPathToFrontend p = Promise.toAffE <<< send p "DOM.pushNodeByPathToFrontend"

domPushNodesByBackendIdsToFrontend :: Foreign -> CDPSession -> Aff Foreign
domPushNodesByBackendIdsToFrontend p = Promise.toAffE <<< send p "DOM.pushNodesByBackendIdsToFrontend"

domQuerySelector :: Foreign -> CDPSession -> Aff Foreign
domQuerySelector p = Promise.toAffE <<< send p "DOM.querySelector"

domQuerySelectorAll :: Foreign -> CDPSession -> Aff Foreign
domQuerySelectorAll p = Promise.toAffE <<< send p "DOM.querySelectorAll"

domGetTopLayerElements :: Foreign -> CDPSession -> Aff Foreign
domGetTopLayerElements p = Promise.toAffE <<< send p "DOM.getTopLayerElements"

domRedo :: CDPSession -> Aff Unit
domRedo = void <<< Promise.toAffE <<< send0 "DOM.redo"

domRemoveAttribute :: Foreign -> CDPSession -> Aff Unit
domRemoveAttribute p = void <<< Promise.toAffE <<< send p "DOM.removeAttribute"

domRemoveNode :: Foreign -> CDPSession -> Aff Unit
domRemoveNode p = void <<< Promise.toAffE <<< send p "DOM.removeNode"

domRequestChildNodes :: Foreign -> CDPSession -> Aff Unit
domRequestChildNodes p = void <<< Promise.toAffE <<< send p "DOM.requestChildNodes"

domRequestNode :: Foreign -> CDPSession -> Aff Foreign
domRequestNode p = Promise.toAffE <<< send p "DOM.requestNode"

domResolveNode :: Foreign -> CDPSession -> Aff Foreign
domResolveNode p = Promise.toAffE <<< send p "DOM.resolveNode"

domSetAttributeValue :: Foreign -> CDPSession -> Aff Unit
domSetAttributeValue p = void <<< Promise.toAffE <<< send p "DOM.setAttributeValue"

domSetAttributesAsText :: Foreign -> CDPSession -> Aff Unit
domSetAttributesAsText p = void <<< Promise.toAffE <<< send p "DOM.setAttributesAsText"

domSetFileInputFiles :: Foreign -> CDPSession -> Aff Unit
domSetFileInputFiles p = void <<< Promise.toAffE <<< send p "DOM.setFileInputFiles"

domSetNodeStackTracesEnabled :: Foreign -> CDPSession -> Aff Unit
domSetNodeStackTracesEnabled p = void <<< Promise.toAffE <<< send p "DOM.setNodeStackTracesEnabled"

domGetNodeStackTraces :: Foreign -> CDPSession -> Aff Foreign
domGetNodeStackTraces p = Promise.toAffE <<< send p "DOM.getNodeStackTraces"

domGetFileInfo :: Foreign -> CDPSession -> Aff Foreign
domGetFileInfo p = Promise.toAffE <<< send p "DOM.getFileInfo"

domSetInspectedNode :: Foreign -> CDPSession -> Aff Unit
domSetInspectedNode p = void <<< Promise.toAffE <<< send p "DOM.setInspectedNode"

domSetNodeName :: Foreign -> CDPSession -> Aff Foreign
domSetNodeName p = Promise.toAffE <<< send p "DOM.setNodeName"

domSetNodeValue :: Foreign -> CDPSession -> Aff Unit
domSetNodeValue p = void <<< Promise.toAffE <<< send p "DOM.setNodeValue"

domSetOuterHTML :: Foreign -> CDPSession -> Aff Unit
domSetOuterHTML p = void <<< Promise.toAffE <<< send p "DOM.setOuterHTML"

domUndo :: CDPSession -> Aff Unit
domUndo = void <<< Promise.toAffE <<< send0 "DOM.undo"

domGetFrameOwner :: Foreign -> CDPSession -> Aff Foreign
domGetFrameOwner p = Promise.toAffE <<< send p "DOM.getFrameOwner"

domGetContainerForNode :: Foreign -> CDPSession -> Aff Foreign
domGetContainerForNode p = Promise.toAffE <<< send p "DOM.getContainerForNode"

domGetQueryingDescendantsForContainer :: Foreign -> CDPSession -> Aff Foreign
domGetQueryingDescendantsForContainer p = Promise.toAffE <<< send p "DOM.getQueryingDescendantsForContainer"

domDebuggerGetEventListeners :: Foreign -> CDPSession -> Aff Foreign
domDebuggerGetEventListeners p = Promise.toAffE <<< send p "DOMDebugger.getEventListeners"

domDebuggerRemoveDOMBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveDOMBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.removeDOMBreakpoint"

domDebuggerRemoveEventListenerBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveEventListenerBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.removeEventListenerBreakpoint"

domDebuggerRemoveInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveInstrumentationBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.removeInstrumentationBreakpoint"

domDebuggerRemoveXHRBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveXHRBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.removeXHRBreakpoint"

domDebuggerSetBreakOnCSPViolation :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetBreakOnCSPViolation p = void <<< Promise.toAffE <<< send p "DOMDebugger.setBreakOnCSPViolation"

domDebuggerSetDOMBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetDOMBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.setDOMBreakpoint"

domDebuggerSetEventListenerBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetEventListenerBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.setEventListenerBreakpoint"

domDebuggerSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetInstrumentationBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.setInstrumentationBreakpoint"

domDebuggerSetXHRBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetXHRBreakpoint p = void <<< Promise.toAffE <<< send p "DOMDebugger.setXHRBreakpoint"

eventBreakpointsSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
eventBreakpointsSetInstrumentationBreakpoint p = void <<< Promise.toAffE <<< send p "EventBreakpoints.setInstrumentationBreakpoint"

eventBreakpointsRemoveInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
eventBreakpointsRemoveInstrumentationBreakpoint p = void <<< Promise.toAffE <<< send p "EventBreakpoints.removeInstrumentationBreakpoint"

domSnapshotDisable :: CDPSession -> Aff Unit
domSnapshotDisable = void <<< Promise.toAffE <<< send0 "DOMSnapshot.disable"

domSnapshotEnable :: CDPSession -> Aff Unit
domSnapshotEnable = void <<< Promise.toAffE <<< send0 "DOMSnapshot.enable"

domSnapshotGetSnapshot :: Foreign -> CDPSession -> Aff Foreign
domSnapshotGetSnapshot p = Promise.toAffE <<< send p "DOMSnapshot.getSnapshot"

domSnapshotCaptureSnapshot :: Foreign -> CDPSession -> Aff Foreign
domSnapshotCaptureSnapshot p = Promise.toAffE <<< send p "DOMSnapshot.captureSnapshot"

domStorageClear :: Foreign -> CDPSession -> Aff Unit
domStorageClear p = void <<< Promise.toAffE <<< send p "DOMStorage.clear"

domStorageDisable :: CDPSession -> Aff Unit
domStorageDisable = void <<< Promise.toAffE <<< send0 "DOMStorage.disable"

domStorageEnable :: CDPSession -> Aff Unit
domStorageEnable = void <<< Promise.toAffE <<< send0 "DOMStorage.enable"

domStorageGetDOMStorageItems :: Foreign -> CDPSession -> Aff Foreign
domStorageGetDOMStorageItems p = Promise.toAffE <<< send p "DOMStorage.getDOMStorageItems"

domStorageRemoveDOMStorageItem :: Foreign -> CDPSession -> Aff Unit
domStorageRemoveDOMStorageItem p = void <<< Promise.toAffE <<< send p "DOMStorage.removeDOMStorageItem"

domStorageSetDOMStorageItem :: Foreign -> CDPSession -> Aff Unit
domStorageSetDOMStorageItem p = void <<< Promise.toAffE <<< send p "DOMStorage.setDOMStorageItem"

databaseDisable :: CDPSession -> Aff Unit
databaseDisable = void <<< Promise.toAffE <<< send0 "Database.disable"

databaseEnable :: CDPSession -> Aff Unit
databaseEnable = void <<< Promise.toAffE <<< send0 "Database.enable"

databaseExecuteSQL :: Foreign -> CDPSession -> Aff Foreign
databaseExecuteSQL p = Promise.toAffE <<< send p "Database.executeSQL"

databaseGetDatabaseTableNames :: Foreign -> CDPSession -> Aff Foreign
databaseGetDatabaseTableNames p = Promise.toAffE <<< send p "Database.getDatabaseTableNames"

deviceOrientationClearDeviceOrientationOverride :: CDPSession -> Aff Unit
deviceOrientationClearDeviceOrientationOverride = void <<< Promise.toAffE <<< send0 "DeviceOrientation.clearDeviceOrientationOverride"

deviceOrientationSetDeviceOrientationOverride :: Foreign -> CDPSession -> Aff Unit
deviceOrientationSetDeviceOrientationOverride p = void <<< Promise.toAffE <<< send p "DeviceOrientation.setDeviceOrientationOverride"

emulationCanEmulate :: Foreign -> CDPSession -> Aff Foreign
emulationCanEmulate p = Promise.toAffE <<< send p "Emulation.canEmulate"

emulationClearDeviceMetricsOverride :: CDPSession -> Aff Unit
emulationClearDeviceMetricsOverride = void <<< Promise.toAffE <<< send0 "Emulation.clearDeviceMetricsOverride"

emulationClearGeolocationOverride :: CDPSession -> Aff Unit
emulationClearGeolocationOverride = void <<< Promise.toAffE <<< send0 "Emulation.clearGeolocationOverride"

emulationResetPageScaleFactor :: CDPSession -> Aff Unit
emulationResetPageScaleFactor = void <<< Promise.toAffE <<< send0 "Emulation.resetPageScaleFactor"

emulationSetFocusEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
emulationSetFocusEmulationEnabled p = void <<< Promise.toAffE <<< send p "Emulation.setFocusEmulationEnabled"

emulationSetAutoDarkModeOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetAutoDarkModeOverride p = void <<< Promise.toAffE <<< send p "Emulation.setAutoDarkModeOverride"

emulationSetCPUThrottlingRate :: Foreign -> CDPSession -> Aff Unit
emulationSetCPUThrottlingRate p = void <<< Promise.toAffE <<< send p "Emulation.setCPUThrottlingRate"

emulationSetDefaultBackgroundColorOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetDefaultBackgroundColorOverride p = void <<< Promise.toAffE <<< send p "Emulation.setDefaultBackgroundColorOverride"

emulationSetDeviceMetricsOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetDeviceMetricsOverride p = void <<< Promise.toAffE <<< send p "Emulation.setDeviceMetricsOverride"

emulationSetScrollbarsHidden :: Foreign -> CDPSession -> Aff Unit
emulationSetScrollbarsHidden p = void <<< Promise.toAffE <<< send p "Emulation.setScrollbarsHidden"

emulationSetDocumentCookieDisabled :: Foreign -> CDPSession -> Aff Unit
emulationSetDocumentCookieDisabled p = void <<< Promise.toAffE <<< send p "Emulation.setDocumentCookieDisabled"

emulationSetEmitTouchEventsForMouse :: Foreign -> CDPSession -> Aff Unit
emulationSetEmitTouchEventsForMouse p = void <<< Promise.toAffE <<< send p "Emulation.setEmitTouchEventsForMouse"

emulationSetEmulatedMedia :: Foreign -> CDPSession -> Aff Unit
emulationSetEmulatedMedia p = void <<< Promise.toAffE <<< send p "Emulation.setEmulatedMedia"

emulationSetEmulatedVisionDeficiency :: Foreign -> CDPSession -> Aff Unit
emulationSetEmulatedVisionDeficiency p = void <<< Promise.toAffE <<< send p "Emulation.setEmulatedVisionDeficiency"

emulationSetGeolocationOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetGeolocationOverride p = void <<< Promise.toAffE <<< send p "Emulation.setGeolocationOverride"

emulationSetIdleOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetIdleOverride p = void <<< Promise.toAffE <<< send p "Emulation.setIdleOverride"

emulationClearIdleOverride :: CDPSession -> Aff Unit
emulationClearIdleOverride = void <<< Promise.toAffE <<< send0 "Emulation.clearIdleOverride"

emulationSetNavigatorOverrides :: Foreign -> CDPSession -> Aff Unit
emulationSetNavigatorOverrides p = void <<< Promise.toAffE <<< send p "Emulation.setNavigatorOverrides"

emulationSetPageScaleFactor :: Foreign -> CDPSession -> Aff Unit
emulationSetPageScaleFactor p = void <<< Promise.toAffE <<< send p "Emulation.setPageScaleFactor"

emulationSetScriptExecutionDisabled :: Foreign -> CDPSession -> Aff Unit
emulationSetScriptExecutionDisabled p = void <<< Promise.toAffE <<< send p "Emulation.setScriptExecutionDisabled"

emulationSetTouchEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
emulationSetTouchEmulationEnabled p = void <<< Promise.toAffE <<< send p "Emulation.setTouchEmulationEnabled"

emulationSetVirtualTimePolicy :: Foreign -> CDPSession -> Aff Foreign
emulationSetVirtualTimePolicy p = Promise.toAffE <<< send p "Emulation.setVirtualTimePolicy"

emulationSetLocaleOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetLocaleOverride p = void <<< Promise.toAffE <<< send p "Emulation.setLocaleOverride"

emulationSetTimezoneOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetTimezoneOverride p = void <<< Promise.toAffE <<< send p "Emulation.setTimezoneOverride"

emulationSetVisibleSize :: Foreign -> CDPSession -> Aff Unit
emulationSetVisibleSize p = void <<< Promise.toAffE <<< send p "Emulation.setVisibleSize"

emulationSetDisabledImageTypes :: Foreign -> CDPSession -> Aff Unit
emulationSetDisabledImageTypes p = void <<< Promise.toAffE <<< send p "Emulation.setDisabledImageTypes"

emulationSetHardwareConcurrencyOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetHardwareConcurrencyOverride p = void <<< Promise.toAffE <<< send p "Emulation.setHardwareConcurrencyOverride"

emulationSetUserAgentOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetUserAgentOverride p = void <<< Promise.toAffE <<< send p "Emulation.setUserAgentOverride"

emulationSetAutomationOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetAutomationOverride p = void <<< Promise.toAffE <<< send p "Emulation.setAutomationOverride"

headlessExperimentalBeginFrame :: Foreign -> CDPSession -> Aff Foreign
headlessExperimentalBeginFrame p = Promise.toAffE <<< send p "HeadlessExperimental.beginFrame"

headlessExperimentalDisable :: CDPSession -> Aff Unit
headlessExperimentalDisable = void <<< Promise.toAffE <<< send0 "HeadlessExperimental.disable"

headlessExperimentalEnable :: CDPSession -> Aff Unit
headlessExperimentalEnable = void <<< Promise.toAffE <<< send0 "HeadlessExperimental.enable"

ioClose :: Foreign -> CDPSession -> Aff Unit
ioClose p = void <<< Promise.toAffE <<< send p "IO.close"

ioRead :: Foreign -> CDPSession -> Aff Foreign
ioRead p = Promise.toAffE <<< send p "IO.read"

ioResolveBlob :: Foreign -> CDPSession -> Aff Foreign
ioResolveBlob p = Promise.toAffE <<< send p "IO.resolveBlob"

indexedDBClearObjectStore :: Foreign -> CDPSession -> Aff Unit
indexedDBClearObjectStore p = void <<< Promise.toAffE <<< send p "IndexedDB.clearObjectStore"

indexedDBDeleteDatabase :: Foreign -> CDPSession -> Aff Unit
indexedDBDeleteDatabase p = void <<< Promise.toAffE <<< send p "IndexedDB.deleteDatabase"

indexedDBDeleteObjectStoreEntries :: Foreign -> CDPSession -> Aff Unit
indexedDBDeleteObjectStoreEntries p = void <<< Promise.toAffE <<< send p "IndexedDB.deleteObjectStoreEntries"

indexedDBDisable :: CDPSession -> Aff Unit
indexedDBDisable = void <<< Promise.toAffE <<< send0 "IndexedDB.disable"

indexedDBEnable :: CDPSession -> Aff Unit
indexedDBEnable = void <<< Promise.toAffE <<< send0 "IndexedDB.enable"

indexedDBRequestData :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestData p = Promise.toAffE <<< send p "IndexedDB.requestData"

indexedDBGetMetadata :: Foreign -> CDPSession -> Aff Foreign
indexedDBGetMetadata p = Promise.toAffE <<< send p "IndexedDB.getMetadata"

indexedDBRequestDatabase :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestDatabase p = Promise.toAffE <<< send p "IndexedDB.requestDatabase"

indexedDBRequestDatabaseNames :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestDatabaseNames p = Promise.toAffE <<< send p "IndexedDB.requestDatabaseNames"

inputDispatchDragEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchDragEvent p = void <<< Promise.toAffE <<< send p "Input.dispatchDragEvent"

inputDispatchKeyEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchKeyEvent p = void <<< Promise.toAffE <<< send p "Input.dispatchKeyEvent"

inputInsertText :: Foreign -> CDPSession -> Aff Unit
inputInsertText p = void <<< Promise.toAffE <<< send p "Input.insertText"

inputImeSetComposition :: Foreign -> CDPSession -> Aff Unit
inputImeSetComposition p = void <<< Promise.toAffE <<< send p "Input.imeSetComposition"

inputDispatchMouseEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchMouseEvent p = void <<< Promise.toAffE <<< send p "Input.dispatchMouseEvent"

inputDispatchTouchEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchTouchEvent p = void <<< Promise.toAffE <<< send p "Input.dispatchTouchEvent"

inputCancelDragging :: CDPSession -> Aff Unit
inputCancelDragging = void <<< Promise.toAffE <<< send0 "Input.cancelDragging"

inputEmulateTouchFromMouseEvent :: Foreign -> CDPSession -> Aff Unit
inputEmulateTouchFromMouseEvent p = void <<< Promise.toAffE <<< send p "Input.emulateTouchFromMouseEvent"

inputSetIgnoreInputEvents :: Foreign -> CDPSession -> Aff Unit
inputSetIgnoreInputEvents p = void <<< Promise.toAffE <<< send p "Input.setIgnoreInputEvents"

inputSetInterceptDrags :: Foreign -> CDPSession -> Aff Unit
inputSetInterceptDrags p = void <<< Promise.toAffE <<< send p "Input.setInterceptDrags"

inputSynthesizePinchGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizePinchGesture p = void <<< Promise.toAffE <<< send p "Input.synthesizePinchGesture"

inputSynthesizeScrollGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizeScrollGesture p = void <<< Promise.toAffE <<< send p "Input.synthesizeScrollGesture"

inputSynthesizeTapGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizeTapGesture p = void <<< Promise.toAffE <<< send p "Input.synthesizeTapGesture"

inspectorDisable :: CDPSession -> Aff Unit
inspectorDisable = void <<< Promise.toAffE <<< send0 "Inspector.disable"

inspectorEnable :: CDPSession -> Aff Unit
inspectorEnable = void <<< Promise.toAffE <<< send0 "Inspector.enable"

layerTreeCompositingReasons :: Foreign -> CDPSession -> Aff Foreign
layerTreeCompositingReasons p = Promise.toAffE <<< send p "LayerTree.compositingReasons"

layerTreeDisable :: CDPSession -> Aff Unit
layerTreeDisable = void <<< Promise.toAffE <<< send0 "LayerTree.disable"

layerTreeEnable :: CDPSession -> Aff Unit
layerTreeEnable = void <<< Promise.toAffE <<< send0 "LayerTree.enable"

layerTreeLoadSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeLoadSnapshot p = Promise.toAffE <<< send p "LayerTree.loadSnapshot"

layerTreeMakeSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeMakeSnapshot p = Promise.toAffE <<< send p "LayerTree.makeSnapshot"

layerTreeProfileSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeProfileSnapshot p = Promise.toAffE <<< send p "LayerTree.profileSnapshot"

layerTreeReleaseSnapshot :: Foreign -> CDPSession -> Aff Unit
layerTreeReleaseSnapshot p = void <<< Promise.toAffE <<< send p "LayerTree.releaseSnapshot"

layerTreeReplaySnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeReplaySnapshot p = Promise.toAffE <<< send p "LayerTree.replaySnapshot"

layerTreeSnapshotCommandLog :: Foreign -> CDPSession -> Aff Foreign
layerTreeSnapshotCommandLog p = Promise.toAffE <<< send p "LayerTree.snapshotCommandLog"

logClear :: CDPSession -> Aff Unit
logClear = void <<< Promise.toAffE <<< send0 "Log.clear"

logDisable :: CDPSession -> Aff Unit
logDisable = void <<< Promise.toAffE <<< send0 "Log.disable"

logEnable :: CDPSession -> Aff Unit
logEnable = void <<< Promise.toAffE <<< send0 "Log.enable"

logStartViolationsReport :: Foreign -> CDPSession -> Aff Unit
logStartViolationsReport p = void <<< Promise.toAffE <<< send p "Log.startViolationsReport"

logStopViolationsReport :: CDPSession -> Aff Unit
logStopViolationsReport = void <<< Promise.toAffE <<< send0 "Log.stopViolationsReport"

memoryGetDOMCounters :: Foreign -> CDPSession -> Aff Foreign
memoryGetDOMCounters p = Promise.toAffE <<< send p "Memory.getDOMCounters"

memoryPrepareForLeakDetection :: CDPSession -> Aff Unit
memoryPrepareForLeakDetection = void <<< Promise.toAffE <<< send0 "Memory.prepareForLeakDetection"

memoryForciblyPurgeJavaScriptMemory :: CDPSession -> Aff Unit
memoryForciblyPurgeJavaScriptMemory = void <<< Promise.toAffE <<< send0 "Memory.forciblyPurgeJavaScriptMemory"

memorySetPressureNotificationsSuppressed :: Foreign -> CDPSession -> Aff Unit
memorySetPressureNotificationsSuppressed p = void <<< Promise.toAffE <<< send p "Memory.setPressureNotificationsSuppressed"

memorySimulatePressureNotification :: Foreign -> CDPSession -> Aff Unit
memorySimulatePressureNotification p = void <<< Promise.toAffE <<< send p "Memory.simulatePressureNotification"

memoryStartSampling :: Foreign -> CDPSession -> Aff Unit
memoryStartSampling p = void <<< Promise.toAffE <<< send p "Memory.startSampling"

memoryStopSampling :: CDPSession -> Aff Unit
memoryStopSampling = void <<< Promise.toAffE <<< send0 "Memory.stopSampling"

memoryGetAllTimeSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetAllTimeSamplingProfile p = Promise.toAffE <<< send p "Memory.getAllTimeSamplingProfile"

memoryGetBrowserSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetBrowserSamplingProfile p = Promise.toAffE <<< send p "Memory.getBrowserSamplingProfile"

memoryGetSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetSamplingProfile p = Promise.toAffE <<< send p "Memory.getSamplingProfile"

networkSetAcceptedEncodings :: Foreign -> CDPSession -> Aff Unit
networkSetAcceptedEncodings p = void <<< Promise.toAffE <<< send p "Network.setAcceptedEncodings"

networkClearAcceptedEncodingsOverride :: CDPSession -> Aff Unit
networkClearAcceptedEncodingsOverride = void <<< Promise.toAffE <<< send0 "Network.clearAcceptedEncodingsOverride"

networkCanClearBrowserCache :: Foreign -> CDPSession -> Aff Foreign
networkCanClearBrowserCache p = Promise.toAffE <<< send p "Network.canClearBrowserCache"

networkCanClearBrowserCookies :: Foreign -> CDPSession -> Aff Foreign
networkCanClearBrowserCookies p = Promise.toAffE <<< send p "Network.canClearBrowserCookies"

networkCanEmulateNetworkConditions :: Foreign -> CDPSession -> Aff Foreign
networkCanEmulateNetworkConditions p = Promise.toAffE <<< send p "Network.canEmulateNetworkConditions"

networkClearBrowserCache :: CDPSession -> Aff Unit
networkClearBrowserCache = void <<< Promise.toAffE <<< send0 "Network.clearBrowserCache"

networkClearBrowserCookies :: CDPSession -> Aff Unit
networkClearBrowserCookies = void <<< Promise.toAffE <<< send0 "Network.clearBrowserCookies"

networkContinueInterceptedRequest :: Foreign -> CDPSession -> Aff Unit
networkContinueInterceptedRequest p = void <<< Promise.toAffE <<< send p "Network.continueInterceptedRequest"

networkDeleteCookies :: Foreign -> CDPSession -> Aff Unit
networkDeleteCookies p = void <<< Promise.toAffE <<< send p "Network.deleteCookies"

networkDisable :: CDPSession -> Aff Unit
networkDisable = void <<< Promise.toAffE <<< send0 "Network.disable"

networkEmulateNetworkConditions :: Foreign -> CDPSession -> Aff Unit
networkEmulateNetworkConditions p = void <<< Promise.toAffE <<< send p "Network.emulateNetworkConditions"

networkEnable :: Foreign -> CDPSession -> Aff Unit
networkEnable p = void <<< Promise.toAffE <<< send p "Network.enable"

networkGetAllCookies :: Foreign -> CDPSession -> Aff Foreign
networkGetAllCookies p = Promise.toAffE <<< send p "Network.getAllCookies"

networkGetCertificate :: Foreign -> CDPSession -> Aff Foreign
networkGetCertificate p = Promise.toAffE <<< send p "Network.getCertificate"

networkGetCookies :: Foreign -> CDPSession -> Aff Foreign
networkGetCookies p = Promise.toAffE <<< send p "Network.getCookies"

networkGetResponseBody :: Foreign -> CDPSession -> Aff Foreign
networkGetResponseBody p = Promise.toAffE <<< send p "Network.getResponseBody"

networkGetRequestPostData :: Foreign -> CDPSession -> Aff Foreign
networkGetRequestPostData p = Promise.toAffE <<< send p "Network.getRequestPostData"

networkGetResponseBodyForInterception :: Foreign -> CDPSession -> Aff Foreign
networkGetResponseBodyForInterception p = Promise.toAffE <<< send p "Network.getResponseBodyForInterception"

networkTakeResponseBodyForInterceptionAsStream :: Foreign -> CDPSession -> Aff Foreign
networkTakeResponseBodyForInterceptionAsStream p = Promise.toAffE <<< send p "Network.takeResponseBodyForInterceptionAsStream"

networkReplayXHR :: Foreign -> CDPSession -> Aff Unit
networkReplayXHR p = void <<< Promise.toAffE <<< send p "Network.replayXHR"

networkSearchInResponseBody :: Foreign -> CDPSession -> Aff Foreign
networkSearchInResponseBody p = Promise.toAffE <<< send p "Network.searchInResponseBody"

networkSetBlockedURLs :: Foreign -> CDPSession -> Aff Unit
networkSetBlockedURLs p = void <<< Promise.toAffE <<< send p "Network.setBlockedURLs"

networkSetBypassServiceWorker :: Foreign -> CDPSession -> Aff Unit
networkSetBypassServiceWorker p = void <<< Promise.toAffE <<< send p "Network.setBypassServiceWorker"

networkSetCacheDisabled :: Foreign -> CDPSession -> Aff Unit
networkSetCacheDisabled p = void <<< Promise.toAffE <<< send p "Network.setCacheDisabled"

networkSetCookie :: Foreign -> CDPSession -> Aff Foreign
networkSetCookie p = Promise.toAffE <<< send p "Network.setCookie"

networkSetCookies :: Foreign -> CDPSession -> Aff Unit
networkSetCookies p = void <<< Promise.toAffE <<< send p "Network.setCookies"

networkSetExtraHTTPHeaders :: Foreign -> CDPSession -> Aff Unit
networkSetExtraHTTPHeaders p = void <<< Promise.toAffE <<< send p "Network.setExtraHTTPHeaders"

networkSetAttachDebugStack :: Foreign -> CDPSession -> Aff Unit
networkSetAttachDebugStack p = void <<< Promise.toAffE <<< send p "Network.setAttachDebugStack"

networkSetRequestInterception :: Foreign -> CDPSession -> Aff Unit
networkSetRequestInterception p = void <<< Promise.toAffE <<< send p "Network.setRequestInterception"

networkSetUserAgentOverride :: Foreign -> CDPSession -> Aff Unit
networkSetUserAgentOverride p = void <<< Promise.toAffE <<< send p "Network.setUserAgentOverride"

networkGetSecurityIsolationStatus :: Foreign -> CDPSession -> Aff Foreign
networkGetSecurityIsolationStatus p = Promise.toAffE <<< send p "Network.getSecurityIsolationStatus"

networkEnableReportingApi :: Foreign -> CDPSession -> Aff Unit
networkEnableReportingApi p = void <<< Promise.toAffE <<< send p "Network.enableReportingApi"

networkLoadNetworkResource :: Foreign -> CDPSession -> Aff Foreign
networkLoadNetworkResource p = Promise.toAffE <<< send p "Network.loadNetworkResource"

overlayDisable :: CDPSession -> Aff Unit
overlayDisable = void <<< Promise.toAffE <<< send0 "Overlay.disable"

overlayEnable :: CDPSession -> Aff Unit
overlayEnable = void <<< Promise.toAffE <<< send0 "Overlay.enable"

overlayGetHighlightObjectForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetHighlightObjectForTest p = Promise.toAffE <<< send p "Overlay.getHighlightObjectForTest"

overlayGetGridHighlightObjectsForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetGridHighlightObjectsForTest p = Promise.toAffE <<< send p "Overlay.getGridHighlightObjectsForTest"

overlayGetSourceOrderHighlightObjectForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetSourceOrderHighlightObjectForTest p = Promise.toAffE <<< send p "Overlay.getSourceOrderHighlightObjectForTest"

overlayHideHighlight :: CDPSession -> Aff Unit
overlayHideHighlight = void <<< Promise.toAffE <<< send0 "Overlay.hideHighlight"

overlayHighlightFrame :: Foreign -> CDPSession -> Aff Unit
overlayHighlightFrame p = void <<< Promise.toAffE <<< send p "Overlay.highlightFrame"

overlayHighlightNode :: Foreign -> CDPSession -> Aff Unit
overlayHighlightNode p = void <<< Promise.toAffE <<< send p "Overlay.highlightNode"

overlayHighlightQuad :: Foreign -> CDPSession -> Aff Unit
overlayHighlightQuad p = void <<< Promise.toAffE <<< send p "Overlay.highlightQuad"

overlayHighlightRect :: Foreign -> CDPSession -> Aff Unit
overlayHighlightRect p = void <<< Promise.toAffE <<< send p "Overlay.highlightRect"

overlayHighlightSourceOrder :: Foreign -> CDPSession -> Aff Unit
overlayHighlightSourceOrder p = void <<< Promise.toAffE <<< send p "Overlay.highlightSourceOrder"

overlaySetInspectMode :: Foreign -> CDPSession -> Aff Unit
overlaySetInspectMode p = void <<< Promise.toAffE <<< send p "Overlay.setInspectMode"

overlaySetShowAdHighlights :: Foreign -> CDPSession -> Aff Unit
overlaySetShowAdHighlights p = void <<< Promise.toAffE <<< send p "Overlay.setShowAdHighlights"

overlaySetPausedInDebuggerMessage :: Foreign -> CDPSession -> Aff Unit
overlaySetPausedInDebuggerMessage p = void <<< Promise.toAffE <<< send p "Overlay.setPausedInDebuggerMessage"

overlaySetShowDebugBorders :: Foreign -> CDPSession -> Aff Unit
overlaySetShowDebugBorders p = void <<< Promise.toAffE <<< send p "Overlay.setShowDebugBorders"

overlaySetShowFPSCounter :: Foreign -> CDPSession -> Aff Unit
overlaySetShowFPSCounter p = void <<< Promise.toAffE <<< send p "Overlay.setShowFPSCounter"

overlaySetShowGridOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowGridOverlays p = void <<< Promise.toAffE <<< send p "Overlay.setShowGridOverlays"

overlaySetShowFlexOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowFlexOverlays p = void <<< Promise.toAffE <<< send p "Overlay.setShowFlexOverlays"

overlaySetShowScrollSnapOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowScrollSnapOverlays p = void <<< Promise.toAffE <<< send p "Overlay.setShowScrollSnapOverlays"

overlaySetShowContainerQueryOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowContainerQueryOverlays p = void <<< Promise.toAffE <<< send p "Overlay.setShowContainerQueryOverlays"

overlaySetShowPaintRects :: Foreign -> CDPSession -> Aff Unit
overlaySetShowPaintRects p = void <<< Promise.toAffE <<< send p "Overlay.setShowPaintRects"

overlaySetShowLayoutShiftRegions :: Foreign -> CDPSession -> Aff Unit
overlaySetShowLayoutShiftRegions p = void <<< Promise.toAffE <<< send p "Overlay.setShowLayoutShiftRegions"

overlaySetShowScrollBottleneckRects :: Foreign -> CDPSession -> Aff Unit
overlaySetShowScrollBottleneckRects p = void <<< Promise.toAffE <<< send p "Overlay.setShowScrollBottleneckRects"

overlaySetShowHitTestBorders :: Foreign -> CDPSession -> Aff Unit
overlaySetShowHitTestBorders p = void <<< Promise.toAffE <<< send p "Overlay.setShowHitTestBorders"

overlaySetShowWebVitals :: Foreign -> CDPSession -> Aff Unit
overlaySetShowWebVitals p = void <<< Promise.toAffE <<< send p "Overlay.setShowWebVitals"

overlaySetShowViewportSizeOnResize :: Foreign -> CDPSession -> Aff Unit
overlaySetShowViewportSizeOnResize p = void <<< Promise.toAffE <<< send p "Overlay.setShowViewportSizeOnResize"

overlaySetShowHinge :: Foreign -> CDPSession -> Aff Unit
overlaySetShowHinge p = void <<< Promise.toAffE <<< send p "Overlay.setShowHinge"

overlaySetShowIsolatedElements :: Foreign -> CDPSession -> Aff Unit
overlaySetShowIsolatedElements p = void <<< Promise.toAffE <<< send p "Overlay.setShowIsolatedElements"

pageAddScriptToEvaluateOnLoad :: Foreign -> CDPSession -> Aff Foreign
pageAddScriptToEvaluateOnLoad p = Promise.toAffE <<< send p "Page.addScriptToEvaluateOnLoad"

pageAddScriptToEvaluateOnNewDocument :: Foreign -> CDPSession -> Aff Foreign
pageAddScriptToEvaluateOnNewDocument p = Promise.toAffE <<< send p "Page.addScriptToEvaluateOnNewDocument"

pageBringToFront :: CDPSession -> Aff Unit
pageBringToFront = void <<< Promise.toAffE <<< send0 "Page.bringToFront"

pageCaptureScreenshot :: Foreign -> CDPSession -> Aff Foreign
pageCaptureScreenshot p = Promise.toAffE <<< send p "Page.captureScreenshot"

pageCaptureSnapshot :: Foreign -> CDPSession -> Aff Foreign
pageCaptureSnapshot p = Promise.toAffE <<< send p "Page.captureSnapshot"

pageClearDeviceMetricsOverride :: CDPSession -> Aff Unit
pageClearDeviceMetricsOverride = void <<< Promise.toAffE <<< send0 "Page.clearDeviceMetricsOverride"

pageClearDeviceOrientationOverride :: CDPSession -> Aff Unit
pageClearDeviceOrientationOverride = void <<< Promise.toAffE <<< send0 "Page.clearDeviceOrientationOverride"

pageClearGeolocationOverride :: CDPSession -> Aff Unit
pageClearGeolocationOverride = void <<< Promise.toAffE <<< send0 "Page.clearGeolocationOverride"

pageCreateIsolatedWorld :: Foreign -> CDPSession -> Aff Foreign
pageCreateIsolatedWorld p = Promise.toAffE <<< send p "Page.createIsolatedWorld"

pageDeleteCookie :: Foreign -> CDPSession -> Aff Unit
pageDeleteCookie p = void <<< Promise.toAffE <<< send p "Page.deleteCookie"

pageDisable :: CDPSession -> Aff Unit
pageDisable = void <<< Promise.toAffE <<< send0 "Page.disable"

pageEnable :: CDPSession -> Aff Unit
pageEnable = void <<< Promise.toAffE <<< send0 "Page.enable"

pageGetAppManifest :: Foreign -> CDPSession -> Aff Foreign
pageGetAppManifest p = Promise.toAffE <<< send p "Page.getAppManifest"

pageGetInstallabilityErrors :: Foreign -> CDPSession -> Aff Foreign
pageGetInstallabilityErrors p = Promise.toAffE <<< send p "Page.getInstallabilityErrors"

pageGetManifestIcons :: Foreign -> CDPSession -> Aff Foreign
pageGetManifestIcons p = Promise.toAffE <<< send p "Page.getManifestIcons"

pageGetAppId :: Foreign -> CDPSession -> Aff Foreign
pageGetAppId p = Promise.toAffE <<< send p "Page.getAppId"

pageGetAdScriptId :: Foreign -> CDPSession -> Aff Foreign
pageGetAdScriptId p = Promise.toAffE <<< send p "Page.getAdScriptId"

pageGetCookies :: Foreign -> CDPSession -> Aff Foreign
pageGetCookies p = Promise.toAffE <<< send p "Page.getCookies"

pageGetFrameTree :: Foreign -> CDPSession -> Aff Foreign
pageGetFrameTree p = Promise.toAffE <<< send p "Page.getFrameTree"

pageGetLayoutMetrics :: Foreign -> CDPSession -> Aff Foreign
pageGetLayoutMetrics p = Promise.toAffE <<< send p "Page.getLayoutMetrics"

pageGetNavigationHistory :: Foreign -> CDPSession -> Aff Foreign
pageGetNavigationHistory p = Promise.toAffE <<< send p "Page.getNavigationHistory"

pageResetNavigationHistory :: CDPSession -> Aff Unit
pageResetNavigationHistory = void <<< Promise.toAffE <<< send0 "Page.resetNavigationHistory"

pageGetResourceContent :: Foreign -> CDPSession -> Aff Foreign
pageGetResourceContent p = Promise.toAffE <<< send p "Page.getResourceContent"

pageGetResourceTree :: Foreign -> CDPSession -> Aff Foreign
pageGetResourceTree p = Promise.toAffE <<< send p "Page.getResourceTree"

pageHandleJavaScriptDialog :: Foreign -> CDPSession -> Aff Unit
pageHandleJavaScriptDialog p = void <<< Promise.toAffE <<< send p "Page.handleJavaScriptDialog"

pageNavigate :: Foreign -> CDPSession -> Aff Foreign
pageNavigate p = Promise.toAffE <<< send p "Page.navigate"

pageNavigateToHistoryEntry :: Foreign -> CDPSession -> Aff Unit
pageNavigateToHistoryEntry p = void <<< Promise.toAffE <<< send p "Page.navigateToHistoryEntry"

pagePrintToPDF :: Foreign -> CDPSession -> Aff Foreign
pagePrintToPDF p = Promise.toAffE <<< send p "Page.printToPDF"

pageReload :: Foreign -> CDPSession -> Aff Unit
pageReload p = void <<< Promise.toAffE <<< send p "Page.reload"

pageRemoveScriptToEvaluateOnLoad :: Foreign -> CDPSession -> Aff Unit
pageRemoveScriptToEvaluateOnLoad p = void <<< Promise.toAffE <<< send p "Page.removeScriptToEvaluateOnLoad"

pageRemoveScriptToEvaluateOnNewDocument :: Foreign -> CDPSession -> Aff Unit
pageRemoveScriptToEvaluateOnNewDocument p = void <<< Promise.toAffE <<< send p "Page.removeScriptToEvaluateOnNewDocument"

pageScreencastFrameAck :: Foreign -> CDPSession -> Aff Unit
pageScreencastFrameAck p = void <<< Promise.toAffE <<< send p "Page.screencastFrameAck"

pageSearchInResource :: Foreign -> CDPSession -> Aff Foreign
pageSearchInResource p = Promise.toAffE <<< send p "Page.searchInResource"

pageSetAdBlockingEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetAdBlockingEnabled p = void <<< Promise.toAffE <<< send p "Page.setAdBlockingEnabled"

pageSetBypassCSP :: Foreign -> CDPSession -> Aff Unit
pageSetBypassCSP p = void <<< Promise.toAffE <<< send p "Page.setBypassCSP"

pageGetPermissionsPolicyState :: Foreign -> CDPSession -> Aff Foreign
pageGetPermissionsPolicyState p = Promise.toAffE <<< send p "Page.getPermissionsPolicyState"

pageGetOriginTrials :: Foreign -> CDPSession -> Aff Foreign
pageGetOriginTrials p = Promise.toAffE <<< send p "Page.getOriginTrials"

pageSetDeviceMetricsOverride :: Foreign -> CDPSession -> Aff Unit
pageSetDeviceMetricsOverride p = void <<< Promise.toAffE <<< send p "Page.setDeviceMetricsOverride"

pageSetDeviceOrientationOverride :: Foreign -> CDPSession -> Aff Unit
pageSetDeviceOrientationOverride p = void <<< Promise.toAffE <<< send p "Page.setDeviceOrientationOverride"

pageSetFontFamilies :: Foreign -> CDPSession -> Aff Unit
pageSetFontFamilies p = void <<< Promise.toAffE <<< send p "Page.setFontFamilies"

pageSetFontSizes :: Foreign -> CDPSession -> Aff Unit
pageSetFontSizes p = void <<< Promise.toAffE <<< send p "Page.setFontSizes"

pageSetDocumentContent :: Foreign -> CDPSession -> Aff Unit
pageSetDocumentContent p = void <<< Promise.toAffE <<< send p "Page.setDocumentContent"

pageSetDownloadBehavior :: Foreign -> CDPSession -> Aff Unit
pageSetDownloadBehavior p = void <<< Promise.toAffE <<< send p "Page.setDownloadBehavior"

pageSetGeolocationOverride :: Foreign -> CDPSession -> Aff Unit
pageSetGeolocationOverride p = void <<< Promise.toAffE <<< send p "Page.setGeolocationOverride"

pageSetLifecycleEventsEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetLifecycleEventsEnabled p = void <<< Promise.toAffE <<< send p "Page.setLifecycleEventsEnabled"

pageSetTouchEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetTouchEmulationEnabled p = void <<< Promise.toAffE <<< send p "Page.setTouchEmulationEnabled"

pageStartScreencast :: Foreign -> CDPSession -> Aff Unit
pageStartScreencast p = void <<< Promise.toAffE <<< send p "Page.startScreencast"

pageStopLoading :: CDPSession -> Aff Unit
pageStopLoading = void <<< Promise.toAffE <<< send0 "Page.stopLoading"

pageCrash :: CDPSession -> Aff Unit
pageCrash = void <<< Promise.toAffE <<< send0 "Page.crash"

pageClose :: CDPSession -> Aff Unit
pageClose = void <<< Promise.toAffE <<< send0 "Page.close"

pageSetWebLifecycleState :: Foreign -> CDPSession -> Aff Unit
pageSetWebLifecycleState p = void <<< Promise.toAffE <<< send p "Page.setWebLifecycleState"

pageStopScreencast :: CDPSession -> Aff Unit
pageStopScreencast = void <<< Promise.toAffE <<< send0 "Page.stopScreencast"

pageProduceCompilationCache :: Foreign -> CDPSession -> Aff Unit
pageProduceCompilationCache p = void <<< Promise.toAffE <<< send p "Page.produceCompilationCache"

pageAddCompilationCache :: Foreign -> CDPSession -> Aff Unit
pageAddCompilationCache p = void <<< Promise.toAffE <<< send p "Page.addCompilationCache"

pageClearCompilationCache :: CDPSession -> Aff Unit
pageClearCompilationCache = void <<< Promise.toAffE <<< send0 "Page.clearCompilationCache"

pageSetSPCTransactionMode :: Foreign -> CDPSession -> Aff Unit
pageSetSPCTransactionMode p = void <<< Promise.toAffE <<< send p "Page.setSPCTransactionMode"

pageSetRPHRegistrationMode :: Foreign -> CDPSession -> Aff Unit
pageSetRPHRegistrationMode p = void <<< Promise.toAffE <<< send p "Page.setRPHRegistrationMode"

pageGenerateTestReport :: Foreign -> CDPSession -> Aff Unit
pageGenerateTestReport p = void <<< Promise.toAffE <<< send p "Page.generateTestReport"

pageWaitForDebugger :: CDPSession -> Aff Unit
pageWaitForDebugger = void <<< Promise.toAffE <<< send0 "Page.waitForDebugger"

pageSetInterceptFileChooserDialog :: Foreign -> CDPSession -> Aff Unit
pageSetInterceptFileChooserDialog p = void <<< Promise.toAffE <<< send p "Page.setInterceptFileChooserDialog"

pageSetPrerenderingAllowed :: Foreign -> CDPSession -> Aff Unit
pageSetPrerenderingAllowed p = void <<< Promise.toAffE <<< send p "Page.setPrerenderingAllowed"

performanceDisable :: CDPSession -> Aff Unit
performanceDisable = void <<< Promise.toAffE <<< send0 "Performance.disable"

performanceEnable :: Foreign -> CDPSession -> Aff Unit
performanceEnable p = void <<< Promise.toAffE <<< send p "Performance.enable"

performanceSetTimeDomain :: Foreign -> CDPSession -> Aff Unit
performanceSetTimeDomain p = void <<< Promise.toAffE <<< send p "Performance.setTimeDomain"

performanceGetMetrics :: Foreign -> CDPSession -> Aff Foreign
performanceGetMetrics p = Promise.toAffE <<< send p "Performance.getMetrics"

performanceTimelineEnable :: Foreign -> CDPSession -> Aff Unit
performanceTimelineEnable p = void <<< Promise.toAffE <<< send p "PerformanceTimeline.enable"

securityDisable :: CDPSession -> Aff Unit
securityDisable = void <<< Promise.toAffE <<< send0 "Security.disable"

securityEnable :: CDPSession -> Aff Unit
securityEnable = void <<< Promise.toAffE <<< send0 "Security.enable"

securitySetIgnoreCertificateErrors :: Foreign -> CDPSession -> Aff Unit
securitySetIgnoreCertificateErrors p = void <<< Promise.toAffE <<< send p "Security.setIgnoreCertificateErrors"

securityHandleCertificateError :: Foreign -> CDPSession -> Aff Unit
securityHandleCertificateError p = void <<< Promise.toAffE <<< send p "Security.handleCertificateError"

securitySetOverrideCertificateErrors :: Foreign -> CDPSession -> Aff Unit
securitySetOverrideCertificateErrors p = void <<< Promise.toAffE <<< send p "Security.setOverrideCertificateErrors"

serviceWorkerDeliverPushMessage :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDeliverPushMessage p = void <<< Promise.toAffE <<< send p "ServiceWorker.deliverPushMessage"

serviceWorkerDisable :: CDPSession -> Aff Unit
serviceWorkerDisable = void <<< Promise.toAffE <<< send0 "ServiceWorker.disable"

serviceWorkerDispatchSyncEvent :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDispatchSyncEvent p = void <<< Promise.toAffE <<< send p "ServiceWorker.dispatchSyncEvent"

serviceWorkerDispatchPeriodicSyncEvent :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDispatchPeriodicSyncEvent p = void <<< Promise.toAffE <<< send p "ServiceWorker.dispatchPeriodicSyncEvent"

serviceWorkerEnable :: CDPSession -> Aff Unit
serviceWorkerEnable = void <<< Promise.toAffE <<< send0 "ServiceWorker.enable"

serviceWorkerInspectWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerInspectWorker p = void <<< Promise.toAffE <<< send p "ServiceWorker.inspectWorker"

serviceWorkerSetForceUpdateOnPageLoad :: Foreign -> CDPSession -> Aff Unit
serviceWorkerSetForceUpdateOnPageLoad p = void <<< Promise.toAffE <<< send p "ServiceWorker.setForceUpdateOnPageLoad"

serviceWorkerSkipWaiting :: Foreign -> CDPSession -> Aff Unit
serviceWorkerSkipWaiting p = void <<< Promise.toAffE <<< send p "ServiceWorker.skipWaiting"

serviceWorkerStartWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerStartWorker p = void <<< Promise.toAffE <<< send p "ServiceWorker.startWorker"

serviceWorkerStopAllWorkers :: CDPSession -> Aff Unit
serviceWorkerStopAllWorkers = void <<< Promise.toAffE <<< send0 "ServiceWorker.stopAllWorkers"

serviceWorkerStopWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerStopWorker p = void <<< Promise.toAffE <<< send p "ServiceWorker.stopWorker"

serviceWorkerUnregister :: Foreign -> CDPSession -> Aff Unit
serviceWorkerUnregister p = void <<< Promise.toAffE <<< send p "ServiceWorker.unregister"

serviceWorkerUpdateRegistration :: Foreign -> CDPSession -> Aff Unit
serviceWorkerUpdateRegistration p = void <<< Promise.toAffE <<< send p "ServiceWorker.updateRegistration"

storageGetStorageKeyForFrame :: Foreign -> CDPSession -> Aff Foreign
storageGetStorageKeyForFrame p = Promise.toAffE <<< send p "Storage.getStorageKeyForFrame"

storageClearDataForOrigin :: Foreign -> CDPSession -> Aff Unit
storageClearDataForOrigin p = void <<< Promise.toAffE <<< send p "Storage.clearDataForOrigin"

storageClearDataForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageClearDataForStorageKey p = void <<< Promise.toAffE <<< send p "Storage.clearDataForStorageKey"

storageGetCookies :: Foreign -> CDPSession -> Aff Foreign
storageGetCookies p = Promise.toAffE <<< send p "Storage.getCookies"

storageSetCookies :: Foreign -> CDPSession -> Aff Unit
storageSetCookies p = void <<< Promise.toAffE <<< send p "Storage.setCookies"

storageClearCookies :: Foreign -> CDPSession -> Aff Unit
storageClearCookies p = void <<< Promise.toAffE <<< send p "Storage.clearCookies"

storageGetUsageAndQuota :: Foreign -> CDPSession -> Aff Foreign
storageGetUsageAndQuota p = Promise.toAffE <<< send p "Storage.getUsageAndQuota"

storageOverrideQuotaForOrigin :: Foreign -> CDPSession -> Aff Unit
storageOverrideQuotaForOrigin p = void <<< Promise.toAffE <<< send p "Storage.overrideQuotaForOrigin"

storageTrackCacheStorageForOrigin :: Foreign -> CDPSession -> Aff Unit
storageTrackCacheStorageForOrigin p = void <<< Promise.toAffE <<< send p "Storage.trackCacheStorageForOrigin"

storageTrackCacheStorageForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageTrackCacheStorageForStorageKey p = void <<< Promise.toAffE <<< send p "Storage.trackCacheStorageForStorageKey"

storageTrackIndexedDBForOrigin :: Foreign -> CDPSession -> Aff Unit
storageTrackIndexedDBForOrigin p = void <<< Promise.toAffE <<< send p "Storage.trackIndexedDBForOrigin"

storageTrackIndexedDBForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageTrackIndexedDBForStorageKey p = void <<< Promise.toAffE <<< send p "Storage.trackIndexedDBForStorageKey"

storageUntrackCacheStorageForOrigin :: Foreign -> CDPSession -> Aff Unit
storageUntrackCacheStorageForOrigin p = void <<< Promise.toAffE <<< send p "Storage.untrackCacheStorageForOrigin"

storageUntrackCacheStorageForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageUntrackCacheStorageForStorageKey p = void <<< Promise.toAffE <<< send p "Storage.untrackCacheStorageForStorageKey"

storageUntrackIndexedDBForOrigin :: Foreign -> CDPSession -> Aff Unit
storageUntrackIndexedDBForOrigin p = void <<< Promise.toAffE <<< send p "Storage.untrackIndexedDBForOrigin"

storageUntrackIndexedDBForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageUntrackIndexedDBForStorageKey p = void <<< Promise.toAffE <<< send p "Storage.untrackIndexedDBForStorageKey"

storageGetTrustTokens :: Foreign -> CDPSession -> Aff Foreign
storageGetTrustTokens p = Promise.toAffE <<< send p "Storage.getTrustTokens"

storageClearTrustTokens :: Foreign -> CDPSession -> Aff Foreign
storageClearTrustTokens p = Promise.toAffE <<< send p "Storage.clearTrustTokens"

storageGetInterestGroupDetails :: Foreign -> CDPSession -> Aff Foreign
storageGetInterestGroupDetails p = Promise.toAffE <<< send p "Storage.getInterestGroupDetails"

storageSetInterestGroupTracking :: Foreign -> CDPSession -> Aff Unit
storageSetInterestGroupTracking p = void <<< Promise.toAffE <<< send p "Storage.setInterestGroupTracking"

storageGetSharedStorageMetadata :: Foreign -> CDPSession -> Aff Foreign
storageGetSharedStorageMetadata p = Promise.toAffE <<< send p "Storage.getSharedStorageMetadata"

storageGetSharedStorageEntries :: Foreign -> CDPSession -> Aff Foreign
storageGetSharedStorageEntries p = Promise.toAffE <<< send p "Storage.getSharedStorageEntries"

storageSetSharedStorageEntry :: Foreign -> CDPSession -> Aff Unit
storageSetSharedStorageEntry p = void <<< Promise.toAffE <<< send p "Storage.setSharedStorageEntry"

storageDeleteSharedStorageEntry :: Foreign -> CDPSession -> Aff Unit
storageDeleteSharedStorageEntry p = void <<< Promise.toAffE <<< send p "Storage.deleteSharedStorageEntry"

storageClearSharedStorageEntries :: Foreign -> CDPSession -> Aff Unit
storageClearSharedStorageEntries p = void <<< Promise.toAffE <<< send p "Storage.clearSharedStorageEntries"

storageResetSharedStorageBudget :: Foreign -> CDPSession -> Aff Unit
storageResetSharedStorageBudget p = void <<< Promise.toAffE <<< send p "Storage.resetSharedStorageBudget"

storageSetSharedStorageTracking :: Foreign -> CDPSession -> Aff Unit
storageSetSharedStorageTracking p = void <<< Promise.toAffE <<< send p "Storage.setSharedStorageTracking"

storageSetStorageBucketTracking :: Foreign -> CDPSession -> Aff Unit
storageSetStorageBucketTracking p = void <<< Promise.toAffE <<< send p "Storage.setStorageBucketTracking"

storageDeleteStorageBucket :: Foreign -> CDPSession -> Aff Unit
storageDeleteStorageBucket p = void <<< Promise.toAffE <<< send p "Storage.deleteStorageBucket"

storageRunBounceTrackingMitigations :: Foreign -> CDPSession -> Aff Foreign
storageRunBounceTrackingMitigations p = Promise.toAffE <<< send p "Storage.runBounceTrackingMitigations"

storageSetAttributionReportingLocalTestingMode :: Foreign -> CDPSession -> Aff Unit
storageSetAttributionReportingLocalTestingMode p = void <<< Promise.toAffE <<< send p "Storage.setAttributionReportingLocalTestingMode"

storageSetAttributionReportingTracking :: Foreign -> CDPSession -> Aff Unit
storageSetAttributionReportingTracking p = void <<< Promise.toAffE <<< send p "Storage.setAttributionReportingTracking"

systemInfoGetInfo :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetInfo p = Promise.toAffE <<< send p "SystemInfo.getInfo"

systemInfoGetFeatureState :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetFeatureState p = Promise.toAffE <<< send p "SystemInfo.getFeatureState"

systemInfoGetProcessInfo :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetProcessInfo p = Promise.toAffE <<< send p "SystemInfo.getProcessInfo"

targetActivateTarget :: Foreign -> CDPSession -> Aff Unit
targetActivateTarget p = void <<< Promise.toAffE <<< send p "Target.activateTarget"

targetAttachToTarget :: Foreign -> CDPSession -> Aff Foreign
targetAttachToTarget p = Promise.toAffE <<< send p "Target.attachToTarget"

targetAttachToBrowserTarget :: Foreign -> CDPSession -> Aff Foreign
targetAttachToBrowserTarget p = Promise.toAffE <<< send p "Target.attachToBrowserTarget"

targetCloseTarget :: Foreign -> CDPSession -> Aff Foreign
targetCloseTarget p = Promise.toAffE <<< send p "Target.closeTarget"

targetExposeDevToolsProtocol :: Foreign -> CDPSession -> Aff Unit
targetExposeDevToolsProtocol p = void <<< Promise.toAffE <<< send p "Target.exposeDevToolsProtocol"

targetCreateBrowserContext :: Foreign -> CDPSession -> Aff Foreign
targetCreateBrowserContext p = Promise.toAffE <<< send p "Target.createBrowserContext"

targetGetBrowserContexts :: Foreign -> CDPSession -> Aff Foreign
targetGetBrowserContexts p = Promise.toAffE <<< send p "Target.getBrowserContexts"

targetCreateTarget :: Foreign -> CDPSession -> Aff Foreign
targetCreateTarget p = Promise.toAffE <<< send p "Target.createTarget"

targetDetachFromTarget :: Foreign -> CDPSession -> Aff Unit
targetDetachFromTarget p = void <<< Promise.toAffE <<< send p "Target.detachFromTarget"

targetDisposeBrowserContext :: Foreign -> CDPSession -> Aff Unit
targetDisposeBrowserContext p = void <<< Promise.toAffE <<< send p "Target.disposeBrowserContext"

targetGetTargetInfo :: Foreign -> CDPSession -> Aff Foreign
targetGetTargetInfo p = Promise.toAffE <<< send p "Target.getTargetInfo"

targetGetTargets :: Foreign -> CDPSession -> Aff Foreign
targetGetTargets p = Promise.toAffE <<< send p "Target.getTargets"

targetSendMessageToTarget :: Foreign -> CDPSession -> Aff Unit
targetSendMessageToTarget p = void <<< Promise.toAffE <<< send p "Target.Promise.toAffE <<< sendMessageToTarget"

targetSetAutoAttach :: Foreign -> CDPSession -> Aff Unit
targetSetAutoAttach p = void <<< Promise.toAffE <<< send p "Target.setAutoAttach"

targetAutoAttachRelated :: Foreign -> CDPSession -> Aff Unit
targetAutoAttachRelated p = void <<< Promise.toAffE <<< send p "Target.autoAttachRelated"

targetSetDiscoverTargets :: Foreign -> CDPSession -> Aff Unit
targetSetDiscoverTargets p = void <<< Promise.toAffE <<< send p "Target.setDiscoverTargets"

targetSetRemoteLocations :: Foreign -> CDPSession -> Aff Unit
targetSetRemoteLocations p = void <<< Promise.toAffE <<< send p "Target.setRemoteLocations"

tetheringBind :: Foreign -> CDPSession -> Aff Unit
tetheringBind p = void <<< Promise.toAffE <<< send p "Tethering.bind"

tetheringUnbind :: Foreign -> CDPSession -> Aff Unit
tetheringUnbind p = void <<< Promise.toAffE <<< send p "Tethering.unbind"

tracingEnd :: CDPSession -> Aff Unit
tracingEnd = void <<< Promise.toAffE <<< send0 "Tracing.end"

tracingGetCategories :: Foreign -> CDPSession -> Aff Foreign
tracingGetCategories p = Promise.toAffE <<< send p "Tracing.getCategories"

tracingRecordClockSyncMarker :: Foreign -> CDPSession -> Aff Unit
tracingRecordClockSyncMarker p = void <<< Promise.toAffE <<< send p "Tracing.recordClockSyncMarker"

tracingRequestMemoryDump :: Foreign -> CDPSession -> Aff Foreign
tracingRequestMemoryDump p = Promise.toAffE <<< send p "Tracing.requestMemoryDump"

tracingStart :: Foreign -> CDPSession -> Aff Unit
tracingStart p = void <<< Promise.toAffE <<< send p "Tracing.start"

fetchDisable :: CDPSession -> Aff Unit
fetchDisable = void <<< Promise.toAffE <<< send0 "Fetch.disable"

fetchEnable :: Foreign -> CDPSession -> Aff Unit
fetchEnable p = void <<< Promise.toAffE <<< send p "Fetch.enable"

fetchFailRequest :: Foreign -> CDPSession -> Aff Unit
fetchFailRequest p = void <<< Promise.toAffE <<< send p "Fetch.failRequest"

fetchFulfillRequest :: Foreign -> CDPSession -> Aff Unit
fetchFulfillRequest p = void <<< Promise.toAffE <<< send p "Fetch.fulfillRequest"

fetchContinueRequest :: Foreign -> CDPSession -> Aff Unit
fetchContinueRequest p = void <<< Promise.toAffE <<< send p "Fetch.continueRequest"

fetchContinueWithAuth :: Foreign -> CDPSession -> Aff Unit
fetchContinueWithAuth p = void <<< Promise.toAffE <<< send p "Fetch.continueWithAuth"

fetchContinueResponse :: Foreign -> CDPSession -> Aff Unit
fetchContinueResponse p = void <<< Promise.toAffE <<< send p "Fetch.continueResponse"

fetchGetResponseBody :: Foreign -> CDPSession -> Aff Foreign
fetchGetResponseBody p = Promise.toAffE <<< send p "Fetch.getResponseBody"

fetchTakeResponseBodyAsStream :: Foreign -> CDPSession -> Aff Foreign
fetchTakeResponseBodyAsStream p = Promise.toAffE <<< send p "Fetch.takeResponseBodyAsStream"

webAudioEnable :: CDPSession -> Aff Unit
webAudioEnable = void <<< Promise.toAffE <<< send0 "WebAudio.enable"

webAudioDisable :: CDPSession -> Aff Unit
webAudioDisable = void <<< Promise.toAffE <<< send0 "WebAudio.disable"

webAudioGetRealtimeData :: Foreign -> CDPSession -> Aff Foreign
webAudioGetRealtimeData p = Promise.toAffE <<< send p "WebAudio.getRealtimeData"

webAuthnEnable :: Foreign -> CDPSession -> Aff Unit
webAuthnEnable p = void <<< Promise.toAffE <<< send p "WebAuthn.enable"

webAuthnDisable :: CDPSession -> Aff Unit
webAuthnDisable = void <<< Promise.toAffE <<< send0 "WebAuthn.disable"

webAuthnAddVirtualAuthenticator :: Foreign -> CDPSession -> Aff Foreign
webAuthnAddVirtualAuthenticator p = Promise.toAffE <<< send p "WebAuthn.addVirtualAuthenticator"

webAuthnSetResponseOverrideBits :: Foreign -> CDPSession -> Aff Unit
webAuthnSetResponseOverrideBits p = void <<< Promise.toAffE <<< send p "WebAuthn.setResponseOverrideBits"

webAuthnRemoveVirtualAuthenticator :: Foreign -> CDPSession -> Aff Unit
webAuthnRemoveVirtualAuthenticator p = void <<< Promise.toAffE <<< send p "WebAuthn.removeVirtualAuthenticator"

webAuthnAddCredential :: Foreign -> CDPSession -> Aff Unit
webAuthnAddCredential p = void <<< Promise.toAffE <<< send p "WebAuthn.addCredential"

webAuthnGetCredential :: Foreign -> CDPSession -> Aff Foreign
webAuthnGetCredential p = Promise.toAffE <<< send p "WebAuthn.getCredential"

webAuthnGetCredentials :: Foreign -> CDPSession -> Aff Foreign
webAuthnGetCredentials p = Promise.toAffE <<< send p "WebAuthn.getCredentials"

webAuthnRemoveCredential :: Foreign -> CDPSession -> Aff Unit
webAuthnRemoveCredential p = void <<< Promise.toAffE <<< send p "WebAuthn.removeCredential"

webAuthnClearCredentials :: Foreign -> CDPSession -> Aff Unit
webAuthnClearCredentials p = void <<< Promise.toAffE <<< send p "WebAuthn.clearCredentials"

webAuthnSetUserVerified :: Foreign -> CDPSession -> Aff Unit
webAuthnSetUserVerified p = void <<< Promise.toAffE <<< send p "WebAuthn.setUserVerified"

webAuthnSetAutomaticPresenceSimulation :: Foreign -> CDPSession -> Aff Unit
webAuthnSetAutomaticPresenceSimulation p = void <<< Promise.toAffE <<< send p "WebAuthn.setAutomaticPresenceSimulation"

mediaEnable :: CDPSession -> Aff Unit
mediaEnable = void <<< Promise.toAffE <<< send0 "Media.enable"

mediaDisable :: CDPSession -> Aff Unit
mediaDisable = void <<< Promise.toAffE <<< send0 "Media.disable"

deviceAccessEnable :: CDPSession -> Aff Unit
deviceAccessEnable = void <<< Promise.toAffE <<< send0 "DeviceAccess.enable"

deviceAccessDisable :: CDPSession -> Aff Unit
deviceAccessDisable = void <<< Promise.toAffE <<< send0 "DeviceAccess.disable"

deviceAccessSelectPrompt :: Foreign -> CDPSession -> Aff Unit
deviceAccessSelectPrompt p = void <<< Promise.toAffE <<< send p "DeviceAccess.selectPrompt"

deviceAccessCancelPrompt :: Foreign -> CDPSession -> Aff Unit
deviceAccessCancelPrompt p = void <<< Promise.toAffE <<< send p "DeviceAccess.cancelPrompt"

preloadEnable :: CDPSession -> Aff Unit
preloadEnable = void <<< Promise.toAffE <<< send0 "Preload.enable"

preloadDisable :: CDPSession -> Aff Unit
preloadDisable = void <<< Promise.toAffE <<< send0 "Preload.disable"

fedCmEnable :: Foreign -> CDPSession -> Aff Unit
fedCmEnable p = void <<< Promise.toAffE <<< send p "FedCm.enable"

fedCmDisable :: CDPSession -> Aff Unit
fedCmDisable = void <<< Promise.toAffE <<< send0 "FedCm.disable"

fedCmSelectAccount :: Foreign -> CDPSession -> Aff Unit
fedCmSelectAccount p = void <<< Promise.toAffE <<< send p "FedCm.selectAccount"

fedCmDismissDialog :: Foreign -> CDPSession -> Aff Unit
fedCmDismissDialog p = void <<< Promise.toAffE <<< send p "FedCm.dismissDialog"

fedCmResetCooldown :: CDPSession -> Aff Unit
fedCmResetCooldown = void <<< Promise.toAffE <<< send0 "FedCm.resetCooldown"
