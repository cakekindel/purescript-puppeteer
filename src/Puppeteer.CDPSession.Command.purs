module Puppeteer.CDPSession.Command where

import Prelude

import Control.Promise (Promise)
import Control.Promise as Promise
import Effect (Effect)
import Effect.Aff (Aff)
import Foreign (Foreign)
import Puppeteer.Base (CDPSession)
import Puppeteer.FFI as FFI

foreign import send :: Foreign -> String -> CDPSession -> Effect (Promise Foreign)
foreign import send0 :: String -> CDPSession -> Effect (Promise Foreign)

consoleClearMessages :: CDPSession -> Aff Unit
consoleClearMessages = void <<< FFI.promiseToAff <<< send0 "Console.clearMessages"

consoleDisable :: CDPSession -> Aff Unit
consoleDisable = void <<< FFI.promiseToAff <<< send0 "Console.disable"

consoleEnable :: CDPSession -> Aff Unit
consoleEnable = void <<< FFI.promiseToAff <<< send0 "Console.enable"

debuggerContinueToLocation :: Foreign -> CDPSession -> Aff Unit
debuggerContinueToLocation p = void <<< FFI.promiseToAff <<< send p "Debugger.continueToLocation"

debuggerDisable :: CDPSession -> Aff Unit
debuggerDisable = void <<< FFI.promiseToAff <<< send0 "Debugger.disable"

debuggerEnable :: Foreign -> CDPSession -> Aff Foreign
debuggerEnable p = FFI.promiseToAff <<< send p "Debugger.enable"

debuggerEvaluateOnCallFrame :: Foreign -> CDPSession -> Aff Foreign
debuggerEvaluateOnCallFrame p = FFI.promiseToAff <<< send p "Debugger.evaluateOnCallFrame"

debuggerGetPossibleBreakpoints :: Foreign -> CDPSession -> Aff Foreign
debuggerGetPossibleBreakpoints p = FFI.promiseToAff <<< send p "Debugger.getPossibleBreakpoints"

debuggerGetScriptSource :: Foreign -> CDPSession -> Aff Foreign
debuggerGetScriptSource p = FFI.promiseToAff <<< send p "Debugger.getScriptSource"

debuggerDisassembleWasmModule :: Foreign -> CDPSession -> Aff Foreign
debuggerDisassembleWasmModule p = FFI.promiseToAff <<< send p "Debugger.disassembleWasmModule"

debuggerNextWasmDisassemblyChunk :: Foreign -> CDPSession -> Aff Foreign
debuggerNextWasmDisassemblyChunk p = FFI.promiseToAff <<< send p "Debugger.nextWasmDisassemblyChunk"

debuggerGetWasmBytecode :: Foreign -> CDPSession -> Aff Foreign
debuggerGetWasmBytecode p = FFI.promiseToAff <<< send p "Debugger.getWasmBytecode"

debuggerGetStackTrace :: Foreign -> CDPSession -> Aff Foreign
debuggerGetStackTrace p = FFI.promiseToAff <<< send p "Debugger.getStackTrace"

debuggerPause :: CDPSession -> Aff Unit
debuggerPause = void <<< FFI.promiseToAff <<< send0 "Debugger.pause"

debuggerPauseOnAsyncCall :: Foreign -> CDPSession -> Aff Unit
debuggerPauseOnAsyncCall p = void <<< FFI.promiseToAff <<< send p "Debugger.pauseOnAsyncCall"

debuggerRemoveBreakpoint :: Foreign -> CDPSession -> Aff Unit
debuggerRemoveBreakpoint p = void <<< FFI.promiseToAff <<< send p "Debugger.removeBreakpoint"

debuggerRestartFrame :: Foreign -> CDPSession -> Aff Foreign
debuggerRestartFrame p = FFI.promiseToAff <<< send p "Debugger.restartFrame"

debuggerResume :: Foreign -> CDPSession -> Aff Unit
debuggerResume p = void <<< FFI.promiseToAff <<< send p "Debugger.resume"

debuggerSearchInContent :: Foreign -> CDPSession -> Aff Foreign
debuggerSearchInContent p = FFI.promiseToAff <<< send p "Debugger.searchInContent"

debuggerSetAsyncCallStackDepth :: Foreign -> CDPSession -> Aff Unit
debuggerSetAsyncCallStackDepth p = void <<< FFI.promiseToAff <<< send p "Debugger.setAsyncCallStackDepth"

debuggerSetBlackboxPatterns :: Foreign -> CDPSession -> Aff Unit
debuggerSetBlackboxPatterns p = void <<< FFI.promiseToAff <<< send p "Debugger.setBlackboxPatterns"

debuggerSetBlackboxedRanges :: Foreign -> CDPSession -> Aff Unit
debuggerSetBlackboxedRanges p = void <<< FFI.promiseToAff <<< send p "Debugger.setBlackboxedRanges"

debuggerSetBreakpoint :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpoint p = FFI.promiseToAff <<< send p "Debugger.setBreakpoint"

debuggerSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Foreign
debuggerSetInstrumentationBreakpoint p = FFI.promiseToAff <<< send p "Debugger.setInstrumentationBreakpoint"

debuggerSetBreakpointByUrl :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpointByUrl p = FFI.promiseToAff <<< send p "Debugger.setBreakpointByUrl"

debuggerSetBreakpointOnFunctionCall :: Foreign -> CDPSession -> Aff Foreign
debuggerSetBreakpointOnFunctionCall p = FFI.promiseToAff <<< send p "Debugger.setBreakpointOnFunctionCall"

debuggerSetBreakpointsActive :: Foreign -> CDPSession -> Aff Unit
debuggerSetBreakpointsActive p = void <<< FFI.promiseToAff <<< send p "Debugger.setBreakpointsActive"

debuggerSetPauseOnExceptions :: Foreign -> CDPSession -> Aff Unit
debuggerSetPauseOnExceptions p = void <<< FFI.promiseToAff <<< send p "Debugger.setPauseOnExceptions"

debuggerSetReturnValue :: Foreign -> CDPSession -> Aff Unit
debuggerSetReturnValue p = void <<< FFI.promiseToAff <<< send p "Debugger.setReturnValue"

debuggerSetScriptSource :: Foreign -> CDPSession -> Aff Foreign
debuggerSetScriptSource p = FFI.promiseToAff <<< send p "Debugger.setScriptSource"

debuggerSetSkipAllPauses :: Foreign -> CDPSession -> Aff Unit
debuggerSetSkipAllPauses p = void <<< FFI.promiseToAff <<< send p "Debugger.setSkipAllPauses"

debuggerSetVariableValue :: Foreign -> CDPSession -> Aff Unit
debuggerSetVariableValue p = void <<< FFI.promiseToAff <<< send p "Debugger.setVariableValue"

debuggerStepInto :: Foreign -> CDPSession -> Aff Unit
debuggerStepInto p = void <<< FFI.promiseToAff <<< send p "Debugger.stepInto"

debuggerStepOut :: CDPSession -> Aff Unit
debuggerStepOut = void <<< FFI.promiseToAff <<< send0 "Debugger.stepOut"

debuggerStepOver :: Foreign -> CDPSession -> Aff Unit
debuggerStepOver p = void <<< FFI.promiseToAff <<< send p "Debugger.stepOver"

heapProfilerAddInspectedHeapObject :: Foreign -> CDPSession -> Aff Unit
heapProfilerAddInspectedHeapObject p = void <<< FFI.promiseToAff <<< send p "HeapProfiler.addInspectedHeapObject"

heapProfilerCollectGarbage :: CDPSession -> Aff Unit
heapProfilerCollectGarbage = void <<< FFI.promiseToAff <<< send0 "HeapProfiler.collectGarbage"

heapProfilerDisable :: CDPSession -> Aff Unit
heapProfilerDisable = void <<< FFI.promiseToAff <<< send0 "HeapProfiler.disable"

heapProfilerEnable :: CDPSession -> Aff Unit
heapProfilerEnable = void <<< FFI.promiseToAff <<< send0 "HeapProfiler.enable"

heapProfilerGetHeapObjectId :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetHeapObjectId p = FFI.promiseToAff <<< send p "HeapProfiler.getHeapObjectId"

heapProfilerGetObjectByHeapObjectId :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetObjectByHeapObjectId p = FFI.promiseToAff <<< send p "HeapProfiler.getObjectByHeapObjectId"

heapProfilerGetSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
heapProfilerGetSamplingProfile p = FFI.promiseToAff <<< send p "HeapProfiler.getSamplingProfile"

heapProfilerStartSampling :: Foreign -> CDPSession -> Aff Unit
heapProfilerStartSampling p = void <<< FFI.promiseToAff <<< send p "HeapProfiler.startSampling"

heapProfilerStartTrackingHeapObjects :: Foreign -> CDPSession -> Aff Unit
heapProfilerStartTrackingHeapObjects p = void <<< FFI.promiseToAff <<< send p "HeapProfiler.startTrackingHeapObjects"

heapProfilerStopSampling :: Foreign -> CDPSession -> Aff Foreign
heapProfilerStopSampling p = FFI.promiseToAff <<< send p "HeapProfiler.stopSampling"

heapProfilerStopTrackingHeapObjects :: Foreign -> CDPSession -> Aff Unit
heapProfilerStopTrackingHeapObjects p = void <<< FFI.promiseToAff <<< send p "HeapProfiler.stopTrackingHeapObjects"

heapProfilerTakeHeapSnapshot :: Foreign -> CDPSession -> Aff Unit
heapProfilerTakeHeapSnapshot p = void <<< FFI.promiseToAff <<< send p "HeapProfiler.takeHeapSnapshot"

profilerDisable :: CDPSession -> Aff Unit
profilerDisable = void <<< FFI.promiseToAff <<< send0 "Profiler.disable"

profilerEnable :: CDPSession -> Aff Unit
profilerEnable = void <<< FFI.promiseToAff <<< send0 "Profiler.enable"

profilerGetBestEffortCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerGetBestEffortCoverage p = FFI.promiseToAff <<< send p "Profiler.getBestEffortCoverage"

profilerSetSamplingInterval :: Foreign -> CDPSession -> Aff Unit
profilerSetSamplingInterval p = void <<< FFI.promiseToAff <<< send p "Profiler.setSamplingInterval"

profilerStart :: CDPSession -> Aff Unit
profilerStart = void <<< FFI.promiseToAff <<< send0 "Profiler.start"

profilerStartPreciseCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerStartPreciseCoverage p = FFI.promiseToAff <<< send p "Profiler.startPreciseCoverage"

profilerStop :: Foreign -> CDPSession -> Aff Foreign
profilerStop p = FFI.promiseToAff <<< send p "Profiler.stop"

profilerStopPreciseCoverage :: CDPSession -> Aff Unit
profilerStopPreciseCoverage = void <<< FFI.promiseToAff <<< send0 "Profiler.stopPreciseCoverage"

profilerTakePreciseCoverage :: Foreign -> CDPSession -> Aff Foreign
profilerTakePreciseCoverage p = FFI.promiseToAff <<< send p "Profiler.takePreciseCoverage"

runtimeAwaitPromise :: Foreign -> CDPSession -> Aff Foreign
runtimeAwaitPromise p = FFI.promiseToAff <<< send p "Runtime.awaitPromise"

runtimeCallFunctionOn :: Foreign -> CDPSession -> Aff Foreign
runtimeCallFunctionOn p = FFI.promiseToAff <<< send p "Runtime.callFunctionOn"

runtimeCompileScript :: Foreign -> CDPSession -> Aff Foreign
runtimeCompileScript p = FFI.promiseToAff <<< send p "Runtime.compileScript"

runtimeDisable :: CDPSession -> Aff Unit
runtimeDisable = void <<< FFI.promiseToAff <<< send0 "Runtime.disable"

runtimeDiscardConsoleEntries :: CDPSession -> Aff Unit
runtimeDiscardConsoleEntries = void <<< FFI.promiseToAff <<< send0 "Runtime.discardConsoleEntries"

runtimeEnable :: CDPSession -> Aff Unit
runtimeEnable = void <<< FFI.promiseToAff <<< send0 "Runtime.enable"

runtimeEvaluate :: Foreign -> CDPSession -> Aff Foreign
runtimeEvaluate p = FFI.promiseToAff <<< send p "Runtime.evaluate"

runtimeGetIsolateId :: Foreign -> CDPSession -> Aff Foreign
runtimeGetIsolateId p = FFI.promiseToAff <<< send p "Runtime.getIsolateId"

runtimeGetHeapUsage :: Foreign -> CDPSession -> Aff Foreign
runtimeGetHeapUsage p = FFI.promiseToAff <<< send p "Runtime.getHeapUsage"

runtimeGetProperties :: Foreign -> CDPSession -> Aff Foreign
runtimeGetProperties p = FFI.promiseToAff <<< send p "Runtime.getProperties"

runtimeGlobalLexicalScopeNames :: Foreign -> CDPSession -> Aff Foreign
runtimeGlobalLexicalScopeNames p = FFI.promiseToAff <<< send p "Runtime.globalLexicalScopeNames"

runtimeQueryObjects :: Foreign -> CDPSession -> Aff Foreign
runtimeQueryObjects p = FFI.promiseToAff <<< send p "Runtime.queryObjects"

runtimeReleaseObject :: Foreign -> CDPSession -> Aff Unit
runtimeReleaseObject p = void <<< FFI.promiseToAff <<< send p "Runtime.releaseObject"

runtimeReleaseObjectGroup :: Foreign -> CDPSession -> Aff Unit
runtimeReleaseObjectGroup p = void <<< FFI.promiseToAff <<< send p "Runtime.releaseObjectGroup"

runtimeRunIfWaitingForDebugger :: CDPSession -> Aff Unit
runtimeRunIfWaitingForDebugger = void <<< FFI.promiseToAff <<< send0 "Runtime.runIfWaitingForDebugger"

runtimeRunScript :: Foreign -> CDPSession -> Aff Foreign
runtimeRunScript p = FFI.promiseToAff <<< send p "Runtime.runScript"

runtimeSetAsyncCallStackDepth :: Foreign -> CDPSession -> Aff Unit
runtimeSetAsyncCallStackDepth p = void <<< FFI.promiseToAff <<< send p "Runtime.setAsyncCallStackDepth"

runtimeSetCustomObjectFormatterEnabled :: Foreign -> CDPSession -> Aff Unit
runtimeSetCustomObjectFormatterEnabled p = void <<< FFI.promiseToAff <<< send p "Runtime.setCustomObjectFormatterEnabled"

runtimeSetMaxCallStackSizeToCapture :: Foreign -> CDPSession -> Aff Unit
runtimeSetMaxCallStackSizeToCapture p = void <<< FFI.promiseToAff <<< send p "Runtime.setMaxCallStackSizeToCapture"

runtimeTerminateExecution :: CDPSession -> Aff Unit
runtimeTerminateExecution = void <<< FFI.promiseToAff <<< send0 "Runtime.terminateExecution"

runtimeAddBinding :: Foreign -> CDPSession -> Aff Unit
runtimeAddBinding p = void <<< FFI.promiseToAff <<< send p "Runtime.addBinding"

runtimeRemoveBinding :: Foreign -> CDPSession -> Aff Unit
runtimeRemoveBinding p = void <<< FFI.promiseToAff <<< send p "Runtime.removeBinding"

runtimeGetExceptionDetails :: Foreign -> CDPSession -> Aff Foreign
runtimeGetExceptionDetails p = FFI.promiseToAff <<< send p "Runtime.getExceptionDetails"

schemaGetDomains :: Foreign -> CDPSession -> Aff Foreign
schemaGetDomains p = FFI.promiseToAff <<< send p "Schema.getDomains"

accessibilityDisable :: CDPSession -> Aff Unit
accessibilityDisable = void <<< FFI.promiseToAff <<< send0 "Accessibility.disable"

accessibilityEnable :: CDPSession -> Aff Unit
accessibilityEnable = void <<< FFI.promiseToAff <<< send0 "Accessibility.enable"

accessibilityGetPartialAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetPartialAXTree p = FFI.promiseToAff <<< send p "Accessibility.getPartialAXTree"

accessibilityGetFullAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetFullAXTree p = FFI.promiseToAff <<< send p "Accessibility.getFullAXTree"

accessibilityGetRootAXNode :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetRootAXNode p = FFI.promiseToAff <<< send p "Accessibility.getRootAXNode"

accessibilityGetAXNodeAndAncestors :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetAXNodeAndAncestors p = FFI.promiseToAff <<< send p "Accessibility.getAXNodeAndAncestors"

accessibilityGetChildAXNodes :: Foreign -> CDPSession -> Aff Foreign
accessibilityGetChildAXNodes p = FFI.promiseToAff <<< send p "Accessibility.getChildAXNodes"

accessibilityQueryAXTree :: Foreign -> CDPSession -> Aff Foreign
accessibilityQueryAXTree p = FFI.promiseToAff <<< send p "Accessibility.queryAXTree"

animationDisable :: CDPSession -> Aff Unit
animationDisable = void <<< FFI.promiseToAff <<< send0 "Animation.disable"

animationEnable :: CDPSession -> Aff Unit
animationEnable = void <<< FFI.promiseToAff <<< send0 "Animation.enable"

animationGetCurrentTime :: Foreign -> CDPSession -> Aff Foreign
animationGetCurrentTime p = FFI.promiseToAff <<< send p "Animation.getCurrentTime"

animationGetPlaybackRate :: Foreign -> CDPSession -> Aff Foreign
animationGetPlaybackRate p = FFI.promiseToAff <<< send p "Animation.getPlaybackRate"

animationReleaseAnimations :: Foreign -> CDPSession -> Aff Unit
animationReleaseAnimations p = void <<< FFI.promiseToAff <<< send p "Animation.releaseAnimations"

animationResolveAnimation :: Foreign -> CDPSession -> Aff Foreign
animationResolveAnimation p = FFI.promiseToAff <<< send p "Animation.resolveAnimation"

animationSeekAnimations :: Foreign -> CDPSession -> Aff Unit
animationSeekAnimations p = void <<< FFI.promiseToAff <<< send p "Animation.seekAnimations"

animationSetPaused :: Foreign -> CDPSession -> Aff Unit
animationSetPaused p = void <<< FFI.promiseToAff <<< send p "Animation.setPaused"

animationSetPlaybackRate :: Foreign -> CDPSession -> Aff Unit
animationSetPlaybackRate p = void <<< FFI.promiseToAff <<< send p "Animation.setPlaybackRate"

animationSetTiming :: Foreign -> CDPSession -> Aff Unit
animationSetTiming p = void <<< FFI.promiseToAff <<< send p "Animation.setTiming"

auditsGetEncodedResponse :: Foreign -> CDPSession -> Aff Foreign
auditsGetEncodedResponse p = FFI.promiseToAff <<< send p "Audits.getEncodedResponse"

auditsDisable :: CDPSession -> Aff Unit
auditsDisable = void <<< FFI.promiseToAff <<< send0 "Audits.disable"

auditsEnable :: CDPSession -> Aff Unit
auditsEnable = void <<< FFI.promiseToAff <<< send0 "Audits.enable"

auditsCheckContrast :: Foreign -> CDPSession -> Aff Unit
auditsCheckContrast p = void <<< FFI.promiseToAff <<< send p "Audits.checkContrast"

auditsCheckFormsIssues :: Foreign -> CDPSession -> Aff Foreign
auditsCheckFormsIssues p = FFI.promiseToAff <<< send p "Audits.checkFormsIssues"

autofillTrigger :: Foreign -> CDPSession -> Aff Unit
autofillTrigger p = void <<< FFI.promiseToAff <<< send p "Autofill.trigger"

autofillSetAddresses :: Foreign -> CDPSession -> Aff Unit
autofillSetAddresses p = void <<< FFI.promiseToAff <<< send p "Autofill.setAddresses"

backgroundServiceStartObserving :: Foreign -> CDPSession -> Aff Unit
backgroundServiceStartObserving p = void <<< FFI.promiseToAff <<< send p "BackgroundService.startObserving"

backgroundServiceStopObserving :: Foreign -> CDPSession -> Aff Unit
backgroundServiceStopObserving p = void <<< FFI.promiseToAff <<< send p "BackgroundService.stopObserving"

backgroundServiceSetRecording :: Foreign -> CDPSession -> Aff Unit
backgroundServiceSetRecording p = void <<< FFI.promiseToAff <<< send p "BackgroundService.setRecording"

backgroundServiceClearEvents :: Foreign -> CDPSession -> Aff Unit
backgroundServiceClearEvents p = void <<< FFI.promiseToAff <<< send p "BackgroundService.clearEvents"

browserSetPermission :: Foreign -> CDPSession -> Aff Unit
browserSetPermission p = void <<< FFI.promiseToAff <<< send p "Browser.setPermission"

browserGrantPermissions :: Foreign -> CDPSession -> Aff Unit
browserGrantPermissions p = void <<< FFI.promiseToAff <<< send p "Browser.grantPermissions"

browserResetPermissions :: Foreign -> CDPSession -> Aff Unit
browserResetPermissions p = void <<< FFI.promiseToAff <<< send p "Browser.resetPermissions"

browserSetDownloadBehavior :: Foreign -> CDPSession -> Aff Unit
browserSetDownloadBehavior p = void <<< FFI.promiseToAff <<< send p "Browser.setDownloadBehavior"

browserCancelDownload :: Foreign -> CDPSession -> Aff Unit
browserCancelDownload p = void <<< FFI.promiseToAff <<< send p "Browser.cancelDownload"

browserClose :: CDPSession -> Aff Unit
browserClose = void <<< FFI.promiseToAff <<< send0 "Browser.close"

browserCrash :: CDPSession -> Aff Unit
browserCrash = void <<< FFI.promiseToAff <<< send0 "Browser.crash"

browserCrashGpuProcess :: CDPSession -> Aff Unit
browserCrashGpuProcess = void <<< FFI.promiseToAff <<< send0 "Browser.crashGpuProcess"

browserGetVersion :: Foreign -> CDPSession -> Aff Foreign
browserGetVersion p = FFI.promiseToAff <<< send p "Browser.getVersion"

browserGetBrowserCommandLine :: Foreign -> CDPSession -> Aff Foreign
browserGetBrowserCommandLine p = FFI.promiseToAff <<< send p "Browser.getBrowserCommandLine"

browserGetHistograms :: Foreign -> CDPSession -> Aff Foreign
browserGetHistograms p = FFI.promiseToAff <<< send p "Browser.getHistograms"

browserGetHistogram :: Foreign -> CDPSession -> Aff Foreign
browserGetHistogram p = FFI.promiseToAff <<< send p "Browser.getHistogram"

browserGetWindowBounds :: Foreign -> CDPSession -> Aff Foreign
browserGetWindowBounds p = FFI.promiseToAff <<< send p "Browser.getWindowBounds"

browserGetWindowForTarget :: Foreign -> CDPSession -> Aff Foreign
browserGetWindowForTarget p = FFI.promiseToAff <<< send p "Browser.getWindowForTarget"

browserSetWindowBounds :: Foreign -> CDPSession -> Aff Unit
browserSetWindowBounds p = void <<< FFI.promiseToAff <<< send p "Browser.setWindowBounds"

browserSetDockTile :: Foreign -> CDPSession -> Aff Unit
browserSetDockTile p = void <<< FFI.promiseToAff <<< send p "Browser.setDockTile"

browserExecuteBrowserCommand :: Foreign -> CDPSession -> Aff Unit
browserExecuteBrowserCommand p = void <<< FFI.promiseToAff <<< send p "Browser.executeBrowserCommand"

browserAddPrivacySandboxEnrollmentOverride :: Foreign -> CDPSession -> Aff Unit
browserAddPrivacySandboxEnrollmentOverride p = void <<< FFI.promiseToAff <<< send p "Browser.addPrivacySandboxEnrollmentOverride"

cSSAddRule :: Foreign -> CDPSession -> Aff Foreign
cSSAddRule p = FFI.promiseToAff <<< send p "CSS.addRule"

cSSCollectClassNames :: Foreign -> CDPSession -> Aff Foreign
cSSCollectClassNames p = FFI.promiseToAff <<< send p "CSS.collectClassNames"

cSSCreateStyleSheet :: Foreign -> CDPSession -> Aff Foreign
cSSCreateStyleSheet p = FFI.promiseToAff <<< send p "CSS.createStyleSheet"

cSSDisable :: CDPSession -> Aff Unit
cSSDisable = void <<< FFI.promiseToAff <<< send0 "CSS.disable"

cSSEnable :: CDPSession -> Aff Unit
cSSEnable = void <<< FFI.promiseToAff <<< send0 "CSS.enable"

cSSForcePseudoState :: Foreign -> CDPSession -> Aff Unit
cSSForcePseudoState p = void <<< FFI.promiseToAff <<< send p "CSS.forcePseudoState"

cSSGetBackgroundColors :: Foreign -> CDPSession -> Aff Foreign
cSSGetBackgroundColors p = FFI.promiseToAff <<< send p "CSS.getBackgroundColors"

cSSGetComputedStyleForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetComputedStyleForNode p = FFI.promiseToAff <<< send p "CSS.getComputedStyleForNode"

cSSGetInlineStylesForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetInlineStylesForNode p = FFI.promiseToAff <<< send p "CSS.getInlineStylesForNode"

cSSGetMatchedStylesForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetMatchedStylesForNode p = FFI.promiseToAff <<< send p "CSS.getMatchedStylesForNode"

cSSGetMediaQueries :: Foreign -> CDPSession -> Aff Foreign
cSSGetMediaQueries p = FFI.promiseToAff <<< send p "CSS.getMediaQueries"

cSSGetPlatformFontsForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetPlatformFontsForNode p = FFI.promiseToAff <<< send p "CSS.getPlatformFontsForNode"

cSSGetStyleSheetText :: Foreign -> CDPSession -> Aff Foreign
cSSGetStyleSheetText p = FFI.promiseToAff <<< send p "CSS.getStyleSheetText"

cSSGetLayersForNode :: Foreign -> CDPSession -> Aff Foreign
cSSGetLayersForNode p = FFI.promiseToAff <<< send p "CSS.getLayersForNode"

cSSTrackComputedStyleUpdates :: Foreign -> CDPSession -> Aff Unit
cSSTrackComputedStyleUpdates p = void <<< FFI.promiseToAff <<< send p "CSS.trackComputedStyleUpdates"

cSSTakeComputedStyleUpdates :: Foreign -> CDPSession -> Aff Foreign
cSSTakeComputedStyleUpdates p = FFI.promiseToAff <<< send p "CSS.takeComputedStyleUpdates"

cSSSetEffectivePropertyValueForNode :: Foreign -> CDPSession -> Aff Unit
cSSSetEffectivePropertyValueForNode p = void <<< FFI.promiseToAff <<< send p "CSS.setEffectivePropertyValueForNode"

cSSSetKeyframeKey :: Foreign -> CDPSession -> Aff Foreign
cSSSetKeyframeKey p = FFI.promiseToAff <<< send p "CSS.setKeyframeKey"

cSSSetMediaText :: Foreign -> CDPSession -> Aff Foreign
cSSSetMediaText p = FFI.promiseToAff <<< send p "CSS.setMediaText"

cSSSetContainerQueryText :: Foreign -> CDPSession -> Aff Foreign
cSSSetContainerQueryText p = FFI.promiseToAff <<< send p "CSS.setContainerQueryText"

cSSSetSupportsText :: Foreign -> CDPSession -> Aff Foreign
cSSSetSupportsText p = FFI.promiseToAff <<< send p "CSS.setSupportsText"

cSSSetScopeText :: Foreign -> CDPSession -> Aff Foreign
cSSSetScopeText p = FFI.promiseToAff <<< send p "CSS.setScopeText"

cSSSetRuleSelector :: Foreign -> CDPSession -> Aff Foreign
cSSSetRuleSelector p = FFI.promiseToAff <<< send p "CSS.setRuleSelector"

cSSSetStyleSheetText :: Foreign -> CDPSession -> Aff Foreign
cSSSetStyleSheetText p = FFI.promiseToAff <<< send p "CSS.setStyleSheetText"

cSSSetStyleTexts :: Foreign -> CDPSession -> Aff Foreign
cSSSetStyleTexts p = FFI.promiseToAff <<< send p "CSS.setStyleTexts"

cSSStartRuleUsageTracking :: CDPSession -> Aff Unit
cSSStartRuleUsageTracking = void <<< FFI.promiseToAff <<< send0 "CSS.startRuleUsageTracking"

cSSStopRuleUsageTracking :: Foreign -> CDPSession -> Aff Foreign
cSSStopRuleUsageTracking p = FFI.promiseToAff <<< send p "CSS.stopRuleUsageTracking"

cSSTakeCoverageDelta :: Foreign -> CDPSession -> Aff Foreign
cSSTakeCoverageDelta p = FFI.promiseToAff <<< send p "CSS.takeCoverageDelta"

cSSSetLocalFontsEnabled :: Foreign -> CDPSession -> Aff Unit
cSSSetLocalFontsEnabled p = void <<< FFI.promiseToAff <<< send p "CSS.setLocalFontsEnabled"

cacheStorageDeleteCache :: Foreign -> CDPSession -> Aff Unit
cacheStorageDeleteCache p = void <<< FFI.promiseToAff <<< send p "CacheStorage.deleteCache"

cacheStorageDeleteEntry :: Foreign -> CDPSession -> Aff Unit
cacheStorageDeleteEntry p = void <<< FFI.promiseToAff <<< send p "CacheStorage.deleteEntry"

cacheStorageRequestCacheNames :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestCacheNames p = FFI.promiseToAff <<< send p "CacheStorage.requestCacheNames"

cacheStorageRequestCachedResponse :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestCachedResponse p = FFI.promiseToAff <<< send p "CacheStorage.requestCachedResponse"

cacheStorageRequestEntries :: Foreign -> CDPSession -> Aff Foreign
cacheStorageRequestEntries p = FFI.promiseToAff <<< send p "CacheStorage.requestEntries"

castEnable :: Foreign -> CDPSession -> Aff Unit
castEnable p = void <<< FFI.promiseToAff <<< send p "Cast.enable"

castDisable :: CDPSession -> Aff Unit
castDisable = void <<< FFI.promiseToAff <<< send0 "Cast.disable"

castSetSinkToUse :: Foreign -> CDPSession -> Aff Unit
castSetSinkToUse p = void <<< FFI.promiseToAff <<< send p "Cast.setSinkToUse"

castStartDesktopMirroring :: Foreign -> CDPSession -> Aff Unit
castStartDesktopMirroring p = void <<< FFI.promiseToAff <<< send p "Cast.startDesktopMirroring"

castStartTabMirroring :: Foreign -> CDPSession -> Aff Unit
castStartTabMirroring p = void <<< FFI.promiseToAff <<< send p "Cast.startTabMirroring"

castStopCasting :: Foreign -> CDPSession -> Aff Unit
castStopCasting p = void <<< FFI.promiseToAff <<< send p "Cast.stopCasting"

domCollectClassNamesFromSubtree :: Foreign -> CDPSession -> Aff Foreign
domCollectClassNamesFromSubtree p = FFI.promiseToAff <<< send p "DOM.collectClassNamesFromSubtree"

domCopyTo :: Foreign -> CDPSession -> Aff Foreign
domCopyTo p = FFI.promiseToAff <<< send p "DOM.copyTo"

domDescribeNode :: Foreign -> CDPSession -> Aff Foreign
domDescribeNode p = FFI.promiseToAff <<< send p "DOM.describeNode"

domScrollIntoViewIfNeeded :: Foreign -> CDPSession -> Aff Unit
domScrollIntoViewIfNeeded p = void <<< FFI.promiseToAff <<< send p "DOM.scrollIntoViewIfNeeded"

domDisable :: CDPSession -> Aff Unit
domDisable = void <<< FFI.promiseToAff <<< send0 "DOM.disable"

domDiscardSearchResults :: Foreign -> CDPSession -> Aff Unit
domDiscardSearchResults p = void <<< FFI.promiseToAff <<< send p "DOM.discardSearchResults"

domEnable :: Foreign -> CDPSession -> Aff Unit
domEnable p = void <<< FFI.promiseToAff <<< send p "DOM.enable"

domFocus :: Foreign -> CDPSession -> Aff Unit
domFocus p = void <<< FFI.promiseToAff <<< send p "DOM.focus"

domGetAttributes :: Foreign -> CDPSession -> Aff Foreign
domGetAttributes p = FFI.promiseToAff <<< send p "DOM.getAttributes"

domGetBoxModel :: Foreign -> CDPSession -> Aff Foreign
domGetBoxModel p = FFI.promiseToAff <<< send p "DOM.getBoxModel"

domGetContentQuads :: Foreign -> CDPSession -> Aff Foreign
domGetContentQuads p = FFI.promiseToAff <<< send p "DOM.getContentQuads"

domGetDocument :: Foreign -> CDPSession -> Aff Foreign
domGetDocument p = FFI.promiseToAff <<< send p "DOM.getDocument"

domGetFlattenedDocument :: Foreign -> CDPSession -> Aff Foreign
domGetFlattenedDocument p = FFI.promiseToAff <<< send p "DOM.getFlattenedDocument"

domGetNodesForSubtreeByStyle :: Foreign -> CDPSession -> Aff Foreign
domGetNodesForSubtreeByStyle p = FFI.promiseToAff <<< send p "DOM.getNodesForSubtreeByStyle"

domGetNodeForLocation :: Foreign -> CDPSession -> Aff Foreign
domGetNodeForLocation p = FFI.promiseToAff <<< send p "DOM.getNodeForLocation"

domGetOuterHTML :: Foreign -> CDPSession -> Aff Foreign
domGetOuterHTML p = FFI.promiseToAff <<< send p "DOM.getOuterHTML"

domGetRelayoutBoundary :: Foreign -> CDPSession -> Aff Foreign
domGetRelayoutBoundary p = FFI.promiseToAff <<< send p "DOM.getRelayoutBoundary"

domGetSearchResults :: Foreign -> CDPSession -> Aff Foreign
domGetSearchResults p = FFI.promiseToAff <<< send p "DOM.getSearchResults"

domHideHighlight :: CDPSession -> Aff Unit
domHideHighlight = void <<< FFI.promiseToAff <<< send0 "DOM.hideHighlight"

domHighlightNode :: CDPSession -> Aff Unit
domHighlightNode = void <<< FFI.promiseToAff <<< send0 "DOM.highlightNode"

domHighlightRect :: CDPSession -> Aff Unit
domHighlightRect = void <<< FFI.promiseToAff <<< send0 "DOM.highlightRect"

domMarkUndoableState :: CDPSession -> Aff Unit
domMarkUndoableState = void <<< FFI.promiseToAff <<< send0 "DOM.markUndoableState"

domMoveTo :: Foreign -> CDPSession -> Aff Foreign
domMoveTo p = FFI.promiseToAff <<< send p "DOM.moveTo"

domPerformSearch :: Foreign -> CDPSession -> Aff Foreign
domPerformSearch p = FFI.promiseToAff <<< send p "DOM.performSearch"

domPushNodeByPathToFrontend :: Foreign -> CDPSession -> Aff Foreign
domPushNodeByPathToFrontend p = FFI.promiseToAff <<< send p "DOM.pushNodeByPathToFrontend"

domPushNodesByBackendIdsToFrontend :: Foreign -> CDPSession -> Aff Foreign
domPushNodesByBackendIdsToFrontend p = FFI.promiseToAff <<< send p "DOM.pushNodesByBackendIdsToFrontend"

domQuerySelector :: Foreign -> CDPSession -> Aff Foreign
domQuerySelector p = FFI.promiseToAff <<< send p "DOM.querySelector"

domQuerySelectorAll :: Foreign -> CDPSession -> Aff Foreign
domQuerySelectorAll p = FFI.promiseToAff <<< send p "DOM.querySelectorAll"

domGetTopLayerElements :: Foreign -> CDPSession -> Aff Foreign
domGetTopLayerElements p = FFI.promiseToAff <<< send p "DOM.getTopLayerElements"

domRedo :: CDPSession -> Aff Unit
domRedo = void <<< FFI.promiseToAff <<< send0 "DOM.redo"

domRemoveAttribute :: Foreign -> CDPSession -> Aff Unit
domRemoveAttribute p = void <<< FFI.promiseToAff <<< send p "DOM.removeAttribute"

domRemoveNode :: Foreign -> CDPSession -> Aff Unit
domRemoveNode p = void <<< FFI.promiseToAff <<< send p "DOM.removeNode"

domRequestChildNodes :: Foreign -> CDPSession -> Aff Unit
domRequestChildNodes p = void <<< FFI.promiseToAff <<< send p "DOM.requestChildNodes"

domRequestNode :: Foreign -> CDPSession -> Aff Foreign
domRequestNode p = FFI.promiseToAff <<< send p "DOM.requestNode"

domResolveNode :: Foreign -> CDPSession -> Aff Foreign
domResolveNode p = FFI.promiseToAff <<< send p "DOM.resolveNode"

domSetAttributeValue :: Foreign -> CDPSession -> Aff Unit
domSetAttributeValue p = void <<< FFI.promiseToAff <<< send p "DOM.setAttributeValue"

domSetAttributesAsText :: Foreign -> CDPSession -> Aff Unit
domSetAttributesAsText p = void <<< FFI.promiseToAff <<< send p "DOM.setAttributesAsText"

domSetFileInputFiles :: Foreign -> CDPSession -> Aff Unit
domSetFileInputFiles p = void <<< FFI.promiseToAff <<< send p "DOM.setFileInputFiles"

domSetNodeStackTracesEnabled :: Foreign -> CDPSession -> Aff Unit
domSetNodeStackTracesEnabled p = void <<< FFI.promiseToAff <<< send p "DOM.setNodeStackTracesEnabled"

domGetNodeStackTraces :: Foreign -> CDPSession -> Aff Foreign
domGetNodeStackTraces p = FFI.promiseToAff <<< send p "DOM.getNodeStackTraces"

domGetFileInfo :: Foreign -> CDPSession -> Aff Foreign
domGetFileInfo p = FFI.promiseToAff <<< send p "DOM.getFileInfo"

domSetInspectedNode :: Foreign -> CDPSession -> Aff Unit
domSetInspectedNode p = void <<< FFI.promiseToAff <<< send p "DOM.setInspectedNode"

domSetNodeName :: Foreign -> CDPSession -> Aff Foreign
domSetNodeName p = FFI.promiseToAff <<< send p "DOM.setNodeName"

domSetNodeValue :: Foreign -> CDPSession -> Aff Unit
domSetNodeValue p = void <<< FFI.promiseToAff <<< send p "DOM.setNodeValue"

domSetOuterHTML :: Foreign -> CDPSession -> Aff Unit
domSetOuterHTML p = void <<< FFI.promiseToAff <<< send p "DOM.setOuterHTML"

domUndo :: CDPSession -> Aff Unit
domUndo = void <<< FFI.promiseToAff <<< send0 "DOM.undo"

domGetFrameOwner :: Foreign -> CDPSession -> Aff Foreign
domGetFrameOwner p = FFI.promiseToAff <<< send p "DOM.getFrameOwner"

domGetContainerForNode :: Foreign -> CDPSession -> Aff Foreign
domGetContainerForNode p = FFI.promiseToAff <<< send p "DOM.getContainerForNode"

domGetQueryingDescendantsForContainer :: Foreign -> CDPSession -> Aff Foreign
domGetQueryingDescendantsForContainer p = FFI.promiseToAff <<< send p "DOM.getQueryingDescendantsForContainer"

domDebuggerGetEventListeners :: Foreign -> CDPSession -> Aff Foreign
domDebuggerGetEventListeners p = FFI.promiseToAff <<< send p "DOMDebugger.getEventListeners"

domDebuggerRemoveDOMBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveDOMBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.removeDOMBreakpoint"

domDebuggerRemoveEventListenerBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveEventListenerBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.removeEventListenerBreakpoint"

domDebuggerRemoveInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveInstrumentationBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.removeInstrumentationBreakpoint"

domDebuggerRemoveXHRBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerRemoveXHRBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.removeXHRBreakpoint"

domDebuggerSetBreakOnCSPViolation :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetBreakOnCSPViolation p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.setBreakOnCSPViolation"

domDebuggerSetDOMBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetDOMBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.setDOMBreakpoint"

domDebuggerSetEventListenerBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetEventListenerBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.setEventListenerBreakpoint"

domDebuggerSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetInstrumentationBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.setInstrumentationBreakpoint"

domDebuggerSetXHRBreakpoint :: Foreign -> CDPSession -> Aff Unit
domDebuggerSetXHRBreakpoint p = void <<< FFI.promiseToAff <<< send p "DOMDebugger.setXHRBreakpoint"

eventBreakpointsSetInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
eventBreakpointsSetInstrumentationBreakpoint p = void <<< FFI.promiseToAff <<< send p "EventBreakpoints.setInstrumentationBreakpoint"

eventBreakpointsRemoveInstrumentationBreakpoint :: Foreign -> CDPSession -> Aff Unit
eventBreakpointsRemoveInstrumentationBreakpoint p = void <<< FFI.promiseToAff <<< send p "EventBreakpoints.removeInstrumentationBreakpoint"

domSnapshotDisable :: CDPSession -> Aff Unit
domSnapshotDisable = void <<< FFI.promiseToAff <<< send0 "DOMSnapshot.disable"

domSnapshotEnable :: CDPSession -> Aff Unit
domSnapshotEnable = void <<< FFI.promiseToAff <<< send0 "DOMSnapshot.enable"

domSnapshotGetSnapshot :: Foreign -> CDPSession -> Aff Foreign
domSnapshotGetSnapshot p = FFI.promiseToAff <<< send p "DOMSnapshot.getSnapshot"

domSnapshotCaptureSnapshot :: Foreign -> CDPSession -> Aff Foreign
domSnapshotCaptureSnapshot p = FFI.promiseToAff <<< send p "DOMSnapshot.captureSnapshot"

domStorageClear :: Foreign -> CDPSession -> Aff Unit
domStorageClear p = void <<< FFI.promiseToAff <<< send p "DOMStorage.clear"

domStorageDisable :: CDPSession -> Aff Unit
domStorageDisable = void <<< FFI.promiseToAff <<< send0 "DOMStorage.disable"

domStorageEnable :: CDPSession -> Aff Unit
domStorageEnable = void <<< FFI.promiseToAff <<< send0 "DOMStorage.enable"

domStorageGetDOMStorageItems :: Foreign -> CDPSession -> Aff Foreign
domStorageGetDOMStorageItems p = FFI.promiseToAff <<< send p "DOMStorage.getDOMStorageItems"

domStorageRemoveDOMStorageItem :: Foreign -> CDPSession -> Aff Unit
domStorageRemoveDOMStorageItem p = void <<< FFI.promiseToAff <<< send p "DOMStorage.removeDOMStorageItem"

domStorageSetDOMStorageItem :: Foreign -> CDPSession -> Aff Unit
domStorageSetDOMStorageItem p = void <<< FFI.promiseToAff <<< send p "DOMStorage.setDOMStorageItem"

databaseDisable :: CDPSession -> Aff Unit
databaseDisable = void <<< FFI.promiseToAff <<< send0 "Database.disable"

databaseEnable :: CDPSession -> Aff Unit
databaseEnable = void <<< FFI.promiseToAff <<< send0 "Database.enable"

databaseExecuteSQL :: Foreign -> CDPSession -> Aff Foreign
databaseExecuteSQL p = FFI.promiseToAff <<< send p "Database.executeSQL"

databaseGetDatabaseTableNames :: Foreign -> CDPSession -> Aff Foreign
databaseGetDatabaseTableNames p = FFI.promiseToAff <<< send p "Database.getDatabaseTableNames"

deviceOrientationClearDeviceOrientationOverride :: CDPSession -> Aff Unit
deviceOrientationClearDeviceOrientationOverride = void <<< FFI.promiseToAff <<< send0 "DeviceOrientation.clearDeviceOrientationOverride"

deviceOrientationSetDeviceOrientationOverride :: Foreign -> CDPSession -> Aff Unit
deviceOrientationSetDeviceOrientationOverride p = void <<< FFI.promiseToAff <<< send p "DeviceOrientation.setDeviceOrientationOverride"

emulationCanEmulate :: Foreign -> CDPSession -> Aff Foreign
emulationCanEmulate p = FFI.promiseToAff <<< send p "Emulation.canEmulate"

emulationClearDeviceMetricsOverride :: CDPSession -> Aff Unit
emulationClearDeviceMetricsOverride = void <<< FFI.promiseToAff <<< send0 "Emulation.clearDeviceMetricsOverride"

emulationClearGeolocationOverride :: CDPSession -> Aff Unit
emulationClearGeolocationOverride = void <<< FFI.promiseToAff <<< send0 "Emulation.clearGeolocationOverride"

emulationResetPageScaleFactor :: CDPSession -> Aff Unit
emulationResetPageScaleFactor = void <<< FFI.promiseToAff <<< send0 "Emulation.resetPageScaleFactor"

emulationSetFocusEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
emulationSetFocusEmulationEnabled p = void <<< FFI.promiseToAff <<< send p "Emulation.setFocusEmulationEnabled"

emulationSetAutoDarkModeOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetAutoDarkModeOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setAutoDarkModeOverride"

emulationSetCPUThrottlingRate :: Foreign -> CDPSession -> Aff Unit
emulationSetCPUThrottlingRate p = void <<< FFI.promiseToAff <<< send p "Emulation.setCPUThrottlingRate"

emulationSetDefaultBackgroundColorOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetDefaultBackgroundColorOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setDefaultBackgroundColorOverride"

emulationSetDeviceMetricsOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetDeviceMetricsOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setDeviceMetricsOverride"

emulationSetScrollbarsHidden :: Foreign -> CDPSession -> Aff Unit
emulationSetScrollbarsHidden p = void <<< FFI.promiseToAff <<< send p "Emulation.setScrollbarsHidden"

emulationSetDocumentCookieDisabled :: Foreign -> CDPSession -> Aff Unit
emulationSetDocumentCookieDisabled p = void <<< FFI.promiseToAff <<< send p "Emulation.setDocumentCookieDisabled"

emulationSetEmitTouchEventsForMouse :: Foreign -> CDPSession -> Aff Unit
emulationSetEmitTouchEventsForMouse p = void <<< FFI.promiseToAff <<< send p "Emulation.setEmitTouchEventsForMouse"

emulationSetEmulatedMedia :: Foreign -> CDPSession -> Aff Unit
emulationSetEmulatedMedia p = void <<< FFI.promiseToAff <<< send p "Emulation.setEmulatedMedia"

emulationSetEmulatedVisionDeficiency :: Foreign -> CDPSession -> Aff Unit
emulationSetEmulatedVisionDeficiency p = void <<< FFI.promiseToAff <<< send p "Emulation.setEmulatedVisionDeficiency"

emulationSetGeolocationOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetGeolocationOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setGeolocationOverride"

emulationSetIdleOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetIdleOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setIdleOverride"

emulationClearIdleOverride :: CDPSession -> Aff Unit
emulationClearIdleOverride = void <<< FFI.promiseToAff <<< send0 "Emulation.clearIdleOverride"

emulationSetNavigatorOverrides :: Foreign -> CDPSession -> Aff Unit
emulationSetNavigatorOverrides p = void <<< FFI.promiseToAff <<< send p "Emulation.setNavigatorOverrides"

emulationSetPageScaleFactor :: Foreign -> CDPSession -> Aff Unit
emulationSetPageScaleFactor p = void <<< FFI.promiseToAff <<< send p "Emulation.setPageScaleFactor"

emulationSetScriptExecutionDisabled :: Foreign -> CDPSession -> Aff Unit
emulationSetScriptExecutionDisabled p = void <<< FFI.promiseToAff <<< send p "Emulation.setScriptExecutionDisabled"

emulationSetTouchEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
emulationSetTouchEmulationEnabled p = void <<< FFI.promiseToAff <<< send p "Emulation.setTouchEmulationEnabled"

emulationSetVirtualTimePolicy :: Foreign -> CDPSession -> Aff Foreign
emulationSetVirtualTimePolicy p = FFI.promiseToAff <<< send p "Emulation.setVirtualTimePolicy"

emulationSetLocaleOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetLocaleOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setLocaleOverride"

emulationSetTimezoneOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetTimezoneOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setTimezoneOverride"

emulationSetVisibleSize :: Foreign -> CDPSession -> Aff Unit
emulationSetVisibleSize p = void <<< FFI.promiseToAff <<< send p "Emulation.setVisibleSize"

emulationSetDisabledImageTypes :: Foreign -> CDPSession -> Aff Unit
emulationSetDisabledImageTypes p = void <<< FFI.promiseToAff <<< send p "Emulation.setDisabledImageTypes"

emulationSetHardwareConcurrencyOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetHardwareConcurrencyOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setHardwareConcurrencyOverride"

emulationSetUserAgentOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetUserAgentOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setUserAgentOverride"

emulationSetAutomationOverride :: Foreign -> CDPSession -> Aff Unit
emulationSetAutomationOverride p = void <<< FFI.promiseToAff <<< send p "Emulation.setAutomationOverride"

headlessExperimentalBeginFrame :: Foreign -> CDPSession -> Aff Foreign
headlessExperimentalBeginFrame p = FFI.promiseToAff <<< send p "HeadlessExperimental.beginFrame"

headlessExperimentalDisable :: CDPSession -> Aff Unit
headlessExperimentalDisable = void <<< FFI.promiseToAff <<< send0 "HeadlessExperimental.disable"

headlessExperimentalEnable :: CDPSession -> Aff Unit
headlessExperimentalEnable = void <<< FFI.promiseToAff <<< send0 "HeadlessExperimental.enable"

ioClose :: Foreign -> CDPSession -> Aff Unit
ioClose p = void <<< FFI.promiseToAff <<< send p "IO.close"

ioRead :: Foreign -> CDPSession -> Aff Foreign
ioRead p = FFI.promiseToAff <<< send p "IO.read"

ioResolveBlob :: Foreign -> CDPSession -> Aff Foreign
ioResolveBlob p = FFI.promiseToAff <<< send p "IO.resolveBlob"

indexedDBClearObjectStore :: Foreign -> CDPSession -> Aff Unit
indexedDBClearObjectStore p = void <<< FFI.promiseToAff <<< send p "IndexedDB.clearObjectStore"

indexedDBDeleteDatabase :: Foreign -> CDPSession -> Aff Unit
indexedDBDeleteDatabase p = void <<< FFI.promiseToAff <<< send p "IndexedDB.deleteDatabase"

indexedDBDeleteObjectStoreEntries :: Foreign -> CDPSession -> Aff Unit
indexedDBDeleteObjectStoreEntries p = void <<< FFI.promiseToAff <<< send p "IndexedDB.deleteObjectStoreEntries"

indexedDBDisable :: CDPSession -> Aff Unit
indexedDBDisable = void <<< FFI.promiseToAff <<< send0 "IndexedDB.disable"

indexedDBEnable :: CDPSession -> Aff Unit
indexedDBEnable = void <<< FFI.promiseToAff <<< send0 "IndexedDB.enable"

indexedDBRequestData :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestData p = FFI.promiseToAff <<< send p "IndexedDB.requestData"

indexedDBGetMetadata :: Foreign -> CDPSession -> Aff Foreign
indexedDBGetMetadata p = FFI.promiseToAff <<< send p "IndexedDB.getMetadata"

indexedDBRequestDatabase :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestDatabase p = FFI.promiseToAff <<< send p "IndexedDB.requestDatabase"

indexedDBRequestDatabaseNames :: Foreign -> CDPSession -> Aff Foreign
indexedDBRequestDatabaseNames p = FFI.promiseToAff <<< send p "IndexedDB.requestDatabaseNames"

inputDispatchDragEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchDragEvent p = void <<< FFI.promiseToAff <<< send p "Input.dispatchDragEvent"

inputDispatchKeyEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchKeyEvent p = void <<< FFI.promiseToAff <<< send p "Input.dispatchKeyEvent"

inputInsertText :: Foreign -> CDPSession -> Aff Unit
inputInsertText p = void <<< FFI.promiseToAff <<< send p "Input.insertText"

inputImeSetComposition :: Foreign -> CDPSession -> Aff Unit
inputImeSetComposition p = void <<< FFI.promiseToAff <<< send p "Input.imeSetComposition"

inputDispatchMouseEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchMouseEvent p = void <<< FFI.promiseToAff <<< send p "Input.dispatchMouseEvent"

inputDispatchTouchEvent :: Foreign -> CDPSession -> Aff Unit
inputDispatchTouchEvent p = void <<< FFI.promiseToAff <<< send p "Input.dispatchTouchEvent"

inputCancelDragging :: CDPSession -> Aff Unit
inputCancelDragging = void <<< FFI.promiseToAff <<< send0 "Input.cancelDragging"

inputEmulateTouchFromMouseEvent :: Foreign -> CDPSession -> Aff Unit
inputEmulateTouchFromMouseEvent p = void <<< FFI.promiseToAff <<< send p "Input.emulateTouchFromMouseEvent"

inputSetIgnoreInputEvents :: Foreign -> CDPSession -> Aff Unit
inputSetIgnoreInputEvents p = void <<< FFI.promiseToAff <<< send p "Input.setIgnoreInputEvents"

inputSetInterceptDrags :: Foreign -> CDPSession -> Aff Unit
inputSetInterceptDrags p = void <<< FFI.promiseToAff <<< send p "Input.setInterceptDrags"

inputSynthesizePinchGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizePinchGesture p = void <<< FFI.promiseToAff <<< send p "Input.synthesizePinchGesture"

inputSynthesizeScrollGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizeScrollGesture p = void <<< FFI.promiseToAff <<< send p "Input.synthesizeScrollGesture"

inputSynthesizeTapGesture :: Foreign -> CDPSession -> Aff Unit
inputSynthesizeTapGesture p = void <<< FFI.promiseToAff <<< send p "Input.synthesizeTapGesture"

inspectorDisable :: CDPSession -> Aff Unit
inspectorDisable = void <<< FFI.promiseToAff <<< send0 "Inspector.disable"

inspectorEnable :: CDPSession -> Aff Unit
inspectorEnable = void <<< FFI.promiseToAff <<< send0 "Inspector.enable"

layerTreeCompositingReasons :: Foreign -> CDPSession -> Aff Foreign
layerTreeCompositingReasons p = FFI.promiseToAff <<< send p "LayerTree.compositingReasons"

layerTreeDisable :: CDPSession -> Aff Unit
layerTreeDisable = void <<< FFI.promiseToAff <<< send0 "LayerTree.disable"

layerTreeEnable :: CDPSession -> Aff Unit
layerTreeEnable = void <<< FFI.promiseToAff <<< send0 "LayerTree.enable"

layerTreeLoadSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeLoadSnapshot p = FFI.promiseToAff <<< send p "LayerTree.loadSnapshot"

layerTreeMakeSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeMakeSnapshot p = FFI.promiseToAff <<< send p "LayerTree.makeSnapshot"

layerTreeProfileSnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeProfileSnapshot p = FFI.promiseToAff <<< send p "LayerTree.profileSnapshot"

layerTreeReleaseSnapshot :: Foreign -> CDPSession -> Aff Unit
layerTreeReleaseSnapshot p = void <<< FFI.promiseToAff <<< send p "LayerTree.releaseSnapshot"

layerTreeReplaySnapshot :: Foreign -> CDPSession -> Aff Foreign
layerTreeReplaySnapshot p = FFI.promiseToAff <<< send p "LayerTree.replaySnapshot"

layerTreeSnapshotCommandLog :: Foreign -> CDPSession -> Aff Foreign
layerTreeSnapshotCommandLog p = FFI.promiseToAff <<< send p "LayerTree.snapshotCommandLog"

logClear :: CDPSession -> Aff Unit
logClear = void <<< FFI.promiseToAff <<< send0 "Log.clear"

logDisable :: CDPSession -> Aff Unit
logDisable = void <<< FFI.promiseToAff <<< send0 "Log.disable"

logEnable :: CDPSession -> Aff Unit
logEnable = void <<< FFI.promiseToAff <<< send0 "Log.enable"

logStartViolationsReport :: Foreign -> CDPSession -> Aff Unit
logStartViolationsReport p = void <<< FFI.promiseToAff <<< send p "Log.startViolationsReport"

logStopViolationsReport :: CDPSession -> Aff Unit
logStopViolationsReport = void <<< FFI.promiseToAff <<< send0 "Log.stopViolationsReport"

memoryGetDOMCounters :: Foreign -> CDPSession -> Aff Foreign
memoryGetDOMCounters p = FFI.promiseToAff <<< send p "Memory.getDOMCounters"

memoryPrepareForLeakDetection :: CDPSession -> Aff Unit
memoryPrepareForLeakDetection = void <<< FFI.promiseToAff <<< send0 "Memory.prepareForLeakDetection"

memoryForciblyPurgeJavaScriptMemory :: CDPSession -> Aff Unit
memoryForciblyPurgeJavaScriptMemory = void <<< FFI.promiseToAff <<< send0 "Memory.forciblyPurgeJavaScriptMemory"

memorySetPressureNotificationsSuppressed :: Foreign -> CDPSession -> Aff Unit
memorySetPressureNotificationsSuppressed p = void <<< FFI.promiseToAff <<< send p "Memory.setPressureNotificationsSuppressed"

memorySimulatePressureNotification :: Foreign -> CDPSession -> Aff Unit
memorySimulatePressureNotification p = void <<< FFI.promiseToAff <<< send p "Memory.simulatePressureNotification"

memoryStartSampling :: Foreign -> CDPSession -> Aff Unit
memoryStartSampling p = void <<< FFI.promiseToAff <<< send p "Memory.startSampling"

memoryStopSampling :: CDPSession -> Aff Unit
memoryStopSampling = void <<< FFI.promiseToAff <<< send0 "Memory.stopSampling"

memoryGetAllTimeSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetAllTimeSamplingProfile p = FFI.promiseToAff <<< send p "Memory.getAllTimeSamplingProfile"

memoryGetBrowserSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetBrowserSamplingProfile p = FFI.promiseToAff <<< send p "Memory.getBrowserSamplingProfile"

memoryGetSamplingProfile :: Foreign -> CDPSession -> Aff Foreign
memoryGetSamplingProfile p = FFI.promiseToAff <<< send p "Memory.getSamplingProfile"

networkSetAcceptedEncodings :: Foreign -> CDPSession -> Aff Unit
networkSetAcceptedEncodings p = void <<< FFI.promiseToAff <<< send p "Network.setAcceptedEncodings"

networkClearAcceptedEncodingsOverride :: CDPSession -> Aff Unit
networkClearAcceptedEncodingsOverride = void <<< FFI.promiseToAff <<< send0 "Network.clearAcceptedEncodingsOverride"

networkCanClearBrowserCache :: Foreign -> CDPSession -> Aff Foreign
networkCanClearBrowserCache p = FFI.promiseToAff <<< send p "Network.canClearBrowserCache"

networkCanClearBrowserCookies :: Foreign -> CDPSession -> Aff Foreign
networkCanClearBrowserCookies p = FFI.promiseToAff <<< send p "Network.canClearBrowserCookies"

networkCanEmulateNetworkConditions :: Foreign -> CDPSession -> Aff Foreign
networkCanEmulateNetworkConditions p = FFI.promiseToAff <<< send p "Network.canEmulateNetworkConditions"

networkClearBrowserCache :: CDPSession -> Aff Unit
networkClearBrowserCache = void <<< FFI.promiseToAff <<< send0 "Network.clearBrowserCache"

networkClearBrowserCookies :: CDPSession -> Aff Unit
networkClearBrowserCookies = void <<< FFI.promiseToAff <<< send0 "Network.clearBrowserCookies"

networkContinueInterceptedRequest :: Foreign -> CDPSession -> Aff Unit
networkContinueInterceptedRequest p = void <<< FFI.promiseToAff <<< send p "Network.continueInterceptedRequest"

networkDeleteCookies :: Foreign -> CDPSession -> Aff Unit
networkDeleteCookies p = void <<< FFI.promiseToAff <<< send p "Network.deleteCookies"

networkDisable :: CDPSession -> Aff Unit
networkDisable = void <<< FFI.promiseToAff <<< send0 "Network.disable"

networkEmulateNetworkConditions :: Foreign -> CDPSession -> Aff Unit
networkEmulateNetworkConditions p = void <<< FFI.promiseToAff <<< send p "Network.emulateNetworkConditions"

networkEnable :: Foreign -> CDPSession -> Aff Unit
networkEnable p = void <<< FFI.promiseToAff <<< send p "Network.enable"

networkGetAllCookies :: Foreign -> CDPSession -> Aff Foreign
networkGetAllCookies p = FFI.promiseToAff <<< send p "Network.getAllCookies"

networkGetCertificate :: Foreign -> CDPSession -> Aff Foreign
networkGetCertificate p = FFI.promiseToAff <<< send p "Network.getCertificate"

networkGetCookies :: Foreign -> CDPSession -> Aff Foreign
networkGetCookies p = FFI.promiseToAff <<< send p "Network.getCookies"

networkGetResponseBody :: Foreign -> CDPSession -> Aff Foreign
networkGetResponseBody p = FFI.promiseToAff <<< send p "Network.getResponseBody"

networkGetRequestPostData :: Foreign -> CDPSession -> Aff Foreign
networkGetRequestPostData p = FFI.promiseToAff <<< send p "Network.getRequestPostData"

networkGetResponseBodyForInterception :: Foreign -> CDPSession -> Aff Foreign
networkGetResponseBodyForInterception p = FFI.promiseToAff <<< send p "Network.getResponseBodyForInterception"

networkTakeResponseBodyForInterceptionAsStream :: Foreign -> CDPSession -> Aff Foreign
networkTakeResponseBodyForInterceptionAsStream p = FFI.promiseToAff <<< send p "Network.takeResponseBodyForInterceptionAsStream"

networkReplayXHR :: Foreign -> CDPSession -> Aff Unit
networkReplayXHR p = void <<< FFI.promiseToAff <<< send p "Network.replayXHR"

networkSearchInResponseBody :: Foreign -> CDPSession -> Aff Foreign
networkSearchInResponseBody p = FFI.promiseToAff <<< send p "Network.searchInResponseBody"

networkSetBlockedURLs :: Foreign -> CDPSession -> Aff Unit
networkSetBlockedURLs p = void <<< FFI.promiseToAff <<< send p "Network.setBlockedURLs"

networkSetBypassServiceWorker :: Foreign -> CDPSession -> Aff Unit
networkSetBypassServiceWorker p = void <<< FFI.promiseToAff <<< send p "Network.setBypassServiceWorker"

networkSetCacheDisabled :: Foreign -> CDPSession -> Aff Unit
networkSetCacheDisabled p = void <<< FFI.promiseToAff <<< send p "Network.setCacheDisabled"

networkSetCookie :: Foreign -> CDPSession -> Aff Foreign
networkSetCookie p = FFI.promiseToAff <<< send p "Network.setCookie"

networkSetCookies :: Foreign -> CDPSession -> Aff Unit
networkSetCookies p = void <<< FFI.promiseToAff <<< send p "Network.setCookies"

networkSetExtraHTTPHeaders :: Foreign -> CDPSession -> Aff Unit
networkSetExtraHTTPHeaders p = void <<< FFI.promiseToAff <<< send p "Network.setExtraHTTPHeaders"

networkSetAttachDebugStack :: Foreign -> CDPSession -> Aff Unit
networkSetAttachDebugStack p = void <<< FFI.promiseToAff <<< send p "Network.setAttachDebugStack"

networkSetRequestInterception :: Foreign -> CDPSession -> Aff Unit
networkSetRequestInterception p = void <<< FFI.promiseToAff <<< send p "Network.setRequestInterception"

networkSetUserAgentOverride :: Foreign -> CDPSession -> Aff Unit
networkSetUserAgentOverride p = void <<< FFI.promiseToAff <<< send p "Network.setUserAgentOverride"

networkGetSecurityIsolationStatus :: Foreign -> CDPSession -> Aff Foreign
networkGetSecurityIsolationStatus p = FFI.promiseToAff <<< send p "Network.getSecurityIsolationStatus"

networkEnableReportingApi :: Foreign -> CDPSession -> Aff Unit
networkEnableReportingApi p = void <<< FFI.promiseToAff <<< send p "Network.enableReportingApi"

networkLoadNetworkResource :: Foreign -> CDPSession -> Aff Foreign
networkLoadNetworkResource p = FFI.promiseToAff <<< send p "Network.loadNetworkResource"

overlayDisable :: CDPSession -> Aff Unit
overlayDisable = void <<< FFI.promiseToAff <<< send0 "Overlay.disable"

overlayEnable :: CDPSession -> Aff Unit
overlayEnable = void <<< FFI.promiseToAff <<< send0 "Overlay.enable"

overlayGetHighlightObjectForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetHighlightObjectForTest p = FFI.promiseToAff <<< send p "Overlay.getHighlightObjectForTest"

overlayGetGridHighlightObjectsForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetGridHighlightObjectsForTest p = FFI.promiseToAff <<< send p "Overlay.getGridHighlightObjectsForTest"

overlayGetSourceOrderHighlightObjectForTest :: Foreign -> CDPSession -> Aff Foreign
overlayGetSourceOrderHighlightObjectForTest p = FFI.promiseToAff <<< send p "Overlay.getSourceOrderHighlightObjectForTest"

overlayHideHighlight :: CDPSession -> Aff Unit
overlayHideHighlight = void <<< FFI.promiseToAff <<< send0 "Overlay.hideHighlight"

overlayHighlightFrame :: Foreign -> CDPSession -> Aff Unit
overlayHighlightFrame p = void <<< FFI.promiseToAff <<< send p "Overlay.highlightFrame"

overlayHighlightNode :: Foreign -> CDPSession -> Aff Unit
overlayHighlightNode p = void <<< FFI.promiseToAff <<< send p "Overlay.highlightNode"

overlayHighlightQuad :: Foreign -> CDPSession -> Aff Unit
overlayHighlightQuad p = void <<< FFI.promiseToAff <<< send p "Overlay.highlightQuad"

overlayHighlightRect :: Foreign -> CDPSession -> Aff Unit
overlayHighlightRect p = void <<< FFI.promiseToAff <<< send p "Overlay.highlightRect"

overlayHighlightSourceOrder :: Foreign -> CDPSession -> Aff Unit
overlayHighlightSourceOrder p = void <<< FFI.promiseToAff <<< send p "Overlay.highlightSourceOrder"

overlaySetInspectMode :: Foreign -> CDPSession -> Aff Unit
overlaySetInspectMode p = void <<< FFI.promiseToAff <<< send p "Overlay.setInspectMode"

overlaySetShowAdHighlights :: Foreign -> CDPSession -> Aff Unit
overlaySetShowAdHighlights p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowAdHighlights"

overlaySetPausedInDebuggerMessage :: Foreign -> CDPSession -> Aff Unit
overlaySetPausedInDebuggerMessage p = void <<< FFI.promiseToAff <<< send p "Overlay.setPausedInDebuggerMessage"

overlaySetShowDebugBorders :: Foreign -> CDPSession -> Aff Unit
overlaySetShowDebugBorders p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowDebugBorders"

overlaySetShowFPSCounter :: Foreign -> CDPSession -> Aff Unit
overlaySetShowFPSCounter p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowFPSCounter"

overlaySetShowGridOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowGridOverlays p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowGridOverlays"

overlaySetShowFlexOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowFlexOverlays p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowFlexOverlays"

overlaySetShowScrollSnapOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowScrollSnapOverlays p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowScrollSnapOverlays"

overlaySetShowContainerQueryOverlays :: Foreign -> CDPSession -> Aff Unit
overlaySetShowContainerQueryOverlays p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowContainerQueryOverlays"

overlaySetShowPaintRects :: Foreign -> CDPSession -> Aff Unit
overlaySetShowPaintRects p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowPaintRects"

overlaySetShowLayoutShiftRegions :: Foreign -> CDPSession -> Aff Unit
overlaySetShowLayoutShiftRegions p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowLayoutShiftRegions"

overlaySetShowScrollBottleneckRects :: Foreign -> CDPSession -> Aff Unit
overlaySetShowScrollBottleneckRects p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowScrollBottleneckRects"

overlaySetShowHitTestBorders :: Foreign -> CDPSession -> Aff Unit
overlaySetShowHitTestBorders p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowHitTestBorders"

overlaySetShowWebVitals :: Foreign -> CDPSession -> Aff Unit
overlaySetShowWebVitals p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowWebVitals"

overlaySetShowViewportSizeOnResize :: Foreign -> CDPSession -> Aff Unit
overlaySetShowViewportSizeOnResize p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowViewportSizeOnResize"

overlaySetShowHinge :: Foreign -> CDPSession -> Aff Unit
overlaySetShowHinge p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowHinge"

overlaySetShowIsolatedElements :: Foreign -> CDPSession -> Aff Unit
overlaySetShowIsolatedElements p = void <<< FFI.promiseToAff <<< send p "Overlay.setShowIsolatedElements"

pageAddScriptToEvaluateOnLoad :: Foreign -> CDPSession -> Aff Foreign
pageAddScriptToEvaluateOnLoad p = FFI.promiseToAff <<< send p "Page.addScriptToEvaluateOnLoad"

pageAddScriptToEvaluateOnNewDocument :: Foreign -> CDPSession -> Aff Foreign
pageAddScriptToEvaluateOnNewDocument p = FFI.promiseToAff <<< send p "Page.addScriptToEvaluateOnNewDocument"

pageBringToFront :: CDPSession -> Aff Unit
pageBringToFront = void <<< FFI.promiseToAff <<< send0 "Page.bringToFront"

pageCaptureScreenshot :: Foreign -> CDPSession -> Aff Foreign
pageCaptureScreenshot p = FFI.promiseToAff <<< send p "Page.captureScreenshot"

pageCaptureSnapshot :: Foreign -> CDPSession -> Aff Foreign
pageCaptureSnapshot p = FFI.promiseToAff <<< send p "Page.captureSnapshot"

pageClearDeviceMetricsOverride :: CDPSession -> Aff Unit
pageClearDeviceMetricsOverride = void <<< FFI.promiseToAff <<< send0 "Page.clearDeviceMetricsOverride"

pageClearDeviceOrientationOverride :: CDPSession -> Aff Unit
pageClearDeviceOrientationOverride = void <<< FFI.promiseToAff <<< send0 "Page.clearDeviceOrientationOverride"

pageClearGeolocationOverride :: CDPSession -> Aff Unit
pageClearGeolocationOverride = void <<< FFI.promiseToAff <<< send0 "Page.clearGeolocationOverride"

pageCreateIsolatedWorld :: Foreign -> CDPSession -> Aff Foreign
pageCreateIsolatedWorld p = FFI.promiseToAff <<< send p "Page.createIsolatedWorld"

pageDeleteCookie :: Foreign -> CDPSession -> Aff Unit
pageDeleteCookie p = void <<< FFI.promiseToAff <<< send p "Page.deleteCookie"

pageDisable :: CDPSession -> Aff Unit
pageDisable = void <<< FFI.promiseToAff <<< send0 "Page.disable"

pageEnable :: CDPSession -> Aff Unit
pageEnable = void <<< FFI.promiseToAff <<< send0 "Page.enable"

pageGetAppManifest :: Foreign -> CDPSession -> Aff Foreign
pageGetAppManifest p = FFI.promiseToAff <<< send p "Page.getAppManifest"

pageGetInstallabilityErrors :: Foreign -> CDPSession -> Aff Foreign
pageGetInstallabilityErrors p = FFI.promiseToAff <<< send p "Page.getInstallabilityErrors"

pageGetManifestIcons :: Foreign -> CDPSession -> Aff Foreign
pageGetManifestIcons p = FFI.promiseToAff <<< send p "Page.getManifestIcons"

pageGetAppId :: Foreign -> CDPSession -> Aff Foreign
pageGetAppId p = FFI.promiseToAff <<< send p "Page.getAppId"

pageGetAdScriptId :: Foreign -> CDPSession -> Aff Foreign
pageGetAdScriptId p = FFI.promiseToAff <<< send p "Page.getAdScriptId"

pageGetCookies :: Foreign -> CDPSession -> Aff Foreign
pageGetCookies p = FFI.promiseToAff <<< send p "Page.getCookies"

pageGetFrameTree :: Foreign -> CDPSession -> Aff Foreign
pageGetFrameTree p = FFI.promiseToAff <<< send p "Page.getFrameTree"

pageGetLayoutMetrics :: Foreign -> CDPSession -> Aff Foreign
pageGetLayoutMetrics p = FFI.promiseToAff <<< send p "Page.getLayoutMetrics"

pageGetNavigationHistory :: Foreign -> CDPSession -> Aff Foreign
pageGetNavigationHistory p = FFI.promiseToAff <<< send p "Page.getNavigationHistory"

pageResetNavigationHistory :: CDPSession -> Aff Unit
pageResetNavigationHistory = void <<< FFI.promiseToAff <<< send0 "Page.resetNavigationHistory"

pageGetResourceContent :: Foreign -> CDPSession -> Aff Foreign
pageGetResourceContent p = FFI.promiseToAff <<< send p "Page.getResourceContent"

pageGetResourceTree :: Foreign -> CDPSession -> Aff Foreign
pageGetResourceTree p = FFI.promiseToAff <<< send p "Page.getResourceTree"

pageHandleJavaScriptDialog :: Foreign -> CDPSession -> Aff Unit
pageHandleJavaScriptDialog p = void <<< FFI.promiseToAff <<< send p "Page.handleJavaScriptDialog"

pageNavigate :: Foreign -> CDPSession -> Aff Foreign
pageNavigate p = FFI.promiseToAff <<< send p "Page.navigate"

pageNavigateToHistoryEntry :: Foreign -> CDPSession -> Aff Unit
pageNavigateToHistoryEntry p = void <<< FFI.promiseToAff <<< send p "Page.navigateToHistoryEntry"

pagePrintToPDF :: Foreign -> CDPSession -> Aff Foreign
pagePrintToPDF p = FFI.promiseToAff <<< send p "Page.printToPDF"

pageReload :: Foreign -> CDPSession -> Aff Unit
pageReload p = void <<< FFI.promiseToAff <<< send p "Page.reload"

pageRemoveScriptToEvaluateOnLoad :: Foreign -> CDPSession -> Aff Unit
pageRemoveScriptToEvaluateOnLoad p = void <<< FFI.promiseToAff <<< send p "Page.removeScriptToEvaluateOnLoad"

pageRemoveScriptToEvaluateOnNewDocument :: Foreign -> CDPSession -> Aff Unit
pageRemoveScriptToEvaluateOnNewDocument p = void <<< FFI.promiseToAff <<< send p "Page.removeScriptToEvaluateOnNewDocument"

pageScreencastFrameAck :: Foreign -> CDPSession -> Aff Unit
pageScreencastFrameAck p = void <<< FFI.promiseToAff <<< send p "Page.screencastFrameAck"

pageSearchInResource :: Foreign -> CDPSession -> Aff Foreign
pageSearchInResource p = FFI.promiseToAff <<< send p "Page.searchInResource"

pageSetAdBlockingEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetAdBlockingEnabled p = void <<< FFI.promiseToAff <<< send p "Page.setAdBlockingEnabled"

pageSetBypassCSP :: Foreign -> CDPSession -> Aff Unit
pageSetBypassCSP p = void <<< FFI.promiseToAff <<< send p "Page.setBypassCSP"

pageGetPermissionsPolicyState :: Foreign -> CDPSession -> Aff Foreign
pageGetPermissionsPolicyState p = FFI.promiseToAff <<< send p "Page.getPermissionsPolicyState"

pageGetOriginTrials :: Foreign -> CDPSession -> Aff Foreign
pageGetOriginTrials p = FFI.promiseToAff <<< send p "Page.getOriginTrials"

pageSetDeviceMetricsOverride :: Foreign -> CDPSession -> Aff Unit
pageSetDeviceMetricsOverride p = void <<< FFI.promiseToAff <<< send p "Page.setDeviceMetricsOverride"

pageSetDeviceOrientationOverride :: Foreign -> CDPSession -> Aff Unit
pageSetDeviceOrientationOverride p = void <<< FFI.promiseToAff <<< send p "Page.setDeviceOrientationOverride"

pageSetFontFamilies :: Foreign -> CDPSession -> Aff Unit
pageSetFontFamilies p = void <<< FFI.promiseToAff <<< send p "Page.setFontFamilies"

pageSetFontSizes :: Foreign -> CDPSession -> Aff Unit
pageSetFontSizes p = void <<< FFI.promiseToAff <<< send p "Page.setFontSizes"

pageSetDocumentContent :: Foreign -> CDPSession -> Aff Unit
pageSetDocumentContent p = void <<< FFI.promiseToAff <<< send p "Page.setDocumentContent"

pageSetDownloadBehavior :: Foreign -> CDPSession -> Aff Unit
pageSetDownloadBehavior p = void <<< FFI.promiseToAff <<< send p "Page.setDownloadBehavior"

pageSetGeolocationOverride :: Foreign -> CDPSession -> Aff Unit
pageSetGeolocationOverride p = void <<< FFI.promiseToAff <<< send p "Page.setGeolocationOverride"

pageSetLifecycleEventsEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetLifecycleEventsEnabled p = void <<< FFI.promiseToAff <<< send p "Page.setLifecycleEventsEnabled"

pageSetTouchEmulationEnabled :: Foreign -> CDPSession -> Aff Unit
pageSetTouchEmulationEnabled p = void <<< FFI.promiseToAff <<< send p "Page.setTouchEmulationEnabled"

pageStartScreencast :: Foreign -> CDPSession -> Aff Unit
pageStartScreencast p = void <<< FFI.promiseToAff <<< send p "Page.startScreencast"

pageStopLoading :: CDPSession -> Aff Unit
pageStopLoading = void <<< FFI.promiseToAff <<< send0 "Page.stopLoading"

pageCrash :: CDPSession -> Aff Unit
pageCrash = void <<< FFI.promiseToAff <<< send0 "Page.crash"

pageClose :: CDPSession -> Aff Unit
pageClose = void <<< FFI.promiseToAff <<< send0 "Page.close"

pageSetWebLifecycleState :: Foreign -> CDPSession -> Aff Unit
pageSetWebLifecycleState p = void <<< FFI.promiseToAff <<< send p "Page.setWebLifecycleState"

pageStopScreencast :: CDPSession -> Aff Unit
pageStopScreencast = void <<< FFI.promiseToAff <<< send0 "Page.stopScreencast"

pageProduceCompilationCache :: Foreign -> CDPSession -> Aff Unit
pageProduceCompilationCache p = void <<< FFI.promiseToAff <<< send p "Page.produceCompilationCache"

pageAddCompilationCache :: Foreign -> CDPSession -> Aff Unit
pageAddCompilationCache p = void <<< FFI.promiseToAff <<< send p "Page.addCompilationCache"

pageClearCompilationCache :: CDPSession -> Aff Unit
pageClearCompilationCache = void <<< FFI.promiseToAff <<< send0 "Page.clearCompilationCache"

pageSetSPCTransactionMode :: Foreign -> CDPSession -> Aff Unit
pageSetSPCTransactionMode p = void <<< FFI.promiseToAff <<< send p "Page.setSPCTransactionMode"

pageSetRPHRegistrationMode :: Foreign -> CDPSession -> Aff Unit
pageSetRPHRegistrationMode p = void <<< FFI.promiseToAff <<< send p "Page.setRPHRegistrationMode"

pageGenerateTestReport :: Foreign -> CDPSession -> Aff Unit
pageGenerateTestReport p = void <<< FFI.promiseToAff <<< send p "Page.generateTestReport"

pageWaitForDebugger :: CDPSession -> Aff Unit
pageWaitForDebugger = void <<< FFI.promiseToAff <<< send0 "Page.waitForDebugger"

pageSetInterceptFileChooserDialog :: Foreign -> CDPSession -> Aff Unit
pageSetInterceptFileChooserDialog p = void <<< FFI.promiseToAff <<< send p "Page.setInterceptFileChooserDialog"

pageSetPrerenderingAllowed :: Foreign -> CDPSession -> Aff Unit
pageSetPrerenderingAllowed p = void <<< FFI.promiseToAff <<< send p "Page.setPrerenderingAllowed"

performanceDisable :: CDPSession -> Aff Unit
performanceDisable = void <<< FFI.promiseToAff <<< send0 "Performance.disable"

performanceEnable :: Foreign -> CDPSession -> Aff Unit
performanceEnable p = void <<< FFI.promiseToAff <<< send p "Performance.enable"

performanceSetTimeDomain :: Foreign -> CDPSession -> Aff Unit
performanceSetTimeDomain p = void <<< FFI.promiseToAff <<< send p "Performance.setTimeDomain"

performanceGetMetrics :: Foreign -> CDPSession -> Aff Foreign
performanceGetMetrics p = FFI.promiseToAff <<< send p "Performance.getMetrics"

performanceTimelineEnable :: Foreign -> CDPSession -> Aff Unit
performanceTimelineEnable p = void <<< FFI.promiseToAff <<< send p "PerformanceTimeline.enable"

securityDisable :: CDPSession -> Aff Unit
securityDisable = void <<< FFI.promiseToAff <<< send0 "Security.disable"

securityEnable :: CDPSession -> Aff Unit
securityEnable = void <<< FFI.promiseToAff <<< send0 "Security.enable"

securitySetIgnoreCertificateErrors :: Foreign -> CDPSession -> Aff Unit
securitySetIgnoreCertificateErrors p = void <<< FFI.promiseToAff <<< send p "Security.setIgnoreCertificateErrors"

securityHandleCertificateError :: Foreign -> CDPSession -> Aff Unit
securityHandleCertificateError p = void <<< FFI.promiseToAff <<< send p "Security.handleCertificateError"

securitySetOverrideCertificateErrors :: Foreign -> CDPSession -> Aff Unit
securitySetOverrideCertificateErrors p = void <<< FFI.promiseToAff <<< send p "Security.setOverrideCertificateErrors"

serviceWorkerDeliverPushMessage :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDeliverPushMessage p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.deliverPushMessage"

serviceWorkerDisable :: CDPSession -> Aff Unit
serviceWorkerDisable = void <<< FFI.promiseToAff <<< send0 "ServiceWorker.disable"

serviceWorkerDispatchSyncEvent :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDispatchSyncEvent p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.dispatchSyncEvent"

serviceWorkerDispatchPeriodicSyncEvent :: Foreign -> CDPSession -> Aff Unit
serviceWorkerDispatchPeriodicSyncEvent p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.dispatchPeriodicSyncEvent"

serviceWorkerEnable :: CDPSession -> Aff Unit
serviceWorkerEnable = void <<< FFI.promiseToAff <<< send0 "ServiceWorker.enable"

serviceWorkerInspectWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerInspectWorker p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.inspectWorker"

serviceWorkerSetForceUpdateOnPageLoad :: Foreign -> CDPSession -> Aff Unit
serviceWorkerSetForceUpdateOnPageLoad p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.setForceUpdateOnPageLoad"

serviceWorkerSkipWaiting :: Foreign -> CDPSession -> Aff Unit
serviceWorkerSkipWaiting p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.skipWaiting"

serviceWorkerStartWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerStartWorker p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.startWorker"

serviceWorkerStopAllWorkers :: CDPSession -> Aff Unit
serviceWorkerStopAllWorkers = void <<< FFI.promiseToAff <<< send0 "ServiceWorker.stopAllWorkers"

serviceWorkerStopWorker :: Foreign -> CDPSession -> Aff Unit
serviceWorkerStopWorker p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.stopWorker"

serviceWorkerUnregister :: Foreign -> CDPSession -> Aff Unit
serviceWorkerUnregister p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.unregister"

serviceWorkerUpdateRegistration :: Foreign -> CDPSession -> Aff Unit
serviceWorkerUpdateRegistration p = void <<< FFI.promiseToAff <<< send p "ServiceWorker.updateRegistration"

storageGetStorageKeyForFrame :: Foreign -> CDPSession -> Aff Foreign
storageGetStorageKeyForFrame p = FFI.promiseToAff <<< send p "Storage.getStorageKeyForFrame"

storageClearDataForOrigin :: Foreign -> CDPSession -> Aff Unit
storageClearDataForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.clearDataForOrigin"

storageClearDataForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageClearDataForStorageKey p = void <<< FFI.promiseToAff <<< send p "Storage.clearDataForStorageKey"

storageGetCookies :: Foreign -> CDPSession -> Aff Foreign
storageGetCookies p = FFI.promiseToAff <<< send p "Storage.getCookies"

storageSetCookies :: Foreign -> CDPSession -> Aff Unit
storageSetCookies p = void <<< FFI.promiseToAff <<< send p "Storage.setCookies"

storageClearCookies :: Foreign -> CDPSession -> Aff Unit
storageClearCookies p = void <<< FFI.promiseToAff <<< send p "Storage.clearCookies"

storageGetUsageAndQuota :: Foreign -> CDPSession -> Aff Foreign
storageGetUsageAndQuota p = FFI.promiseToAff <<< send p "Storage.getUsageAndQuota"

storageOverrideQuotaForOrigin :: Foreign -> CDPSession -> Aff Unit
storageOverrideQuotaForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.overrideQuotaForOrigin"

storageTrackCacheStorageForOrigin :: Foreign -> CDPSession -> Aff Unit
storageTrackCacheStorageForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.trackCacheStorageForOrigin"

storageTrackCacheStorageForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageTrackCacheStorageForStorageKey p = void <<< FFI.promiseToAff <<< send p "Storage.trackCacheStorageForStorageKey"

storageTrackIndexedDBForOrigin :: Foreign -> CDPSession -> Aff Unit
storageTrackIndexedDBForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.trackIndexedDBForOrigin"

storageTrackIndexedDBForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageTrackIndexedDBForStorageKey p = void <<< FFI.promiseToAff <<< send p "Storage.trackIndexedDBForStorageKey"

storageUntrackCacheStorageForOrigin :: Foreign -> CDPSession -> Aff Unit
storageUntrackCacheStorageForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.untrackCacheStorageForOrigin"

storageUntrackCacheStorageForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageUntrackCacheStorageForStorageKey p = void <<< FFI.promiseToAff <<< send p "Storage.untrackCacheStorageForStorageKey"

storageUntrackIndexedDBForOrigin :: Foreign -> CDPSession -> Aff Unit
storageUntrackIndexedDBForOrigin p = void <<< FFI.promiseToAff <<< send p "Storage.untrackIndexedDBForOrigin"

storageUntrackIndexedDBForStorageKey :: Foreign -> CDPSession -> Aff Unit
storageUntrackIndexedDBForStorageKey p = void <<< FFI.promiseToAff <<< send p "Storage.untrackIndexedDBForStorageKey"

storageGetTrustTokens :: Foreign -> CDPSession -> Aff Foreign
storageGetTrustTokens p = FFI.promiseToAff <<< send p "Storage.getTrustTokens"

storageClearTrustTokens :: Foreign -> CDPSession -> Aff Foreign
storageClearTrustTokens p = FFI.promiseToAff <<< send p "Storage.clearTrustTokens"

storageGetInterestGroupDetails :: Foreign -> CDPSession -> Aff Foreign
storageGetInterestGroupDetails p = FFI.promiseToAff <<< send p "Storage.getInterestGroupDetails"

storageSetInterestGroupTracking :: Foreign -> CDPSession -> Aff Unit
storageSetInterestGroupTracking p = void <<< FFI.promiseToAff <<< send p "Storage.setInterestGroupTracking"

storageGetSharedStorageMetadata :: Foreign -> CDPSession -> Aff Foreign
storageGetSharedStorageMetadata p = FFI.promiseToAff <<< send p "Storage.getSharedStorageMetadata"

storageGetSharedStorageEntries :: Foreign -> CDPSession -> Aff Foreign
storageGetSharedStorageEntries p = FFI.promiseToAff <<< send p "Storage.getSharedStorageEntries"

storageSetSharedStorageEntry :: Foreign -> CDPSession -> Aff Unit
storageSetSharedStorageEntry p = void <<< FFI.promiseToAff <<< send p "Storage.setSharedStorageEntry"

storageDeleteSharedStorageEntry :: Foreign -> CDPSession -> Aff Unit
storageDeleteSharedStorageEntry p = void <<< FFI.promiseToAff <<< send p "Storage.deleteSharedStorageEntry"

storageClearSharedStorageEntries :: Foreign -> CDPSession -> Aff Unit
storageClearSharedStorageEntries p = void <<< FFI.promiseToAff <<< send p "Storage.clearSharedStorageEntries"

storageResetSharedStorageBudget :: Foreign -> CDPSession -> Aff Unit
storageResetSharedStorageBudget p = void <<< FFI.promiseToAff <<< send p "Storage.resetSharedStorageBudget"

storageSetSharedStorageTracking :: Foreign -> CDPSession -> Aff Unit
storageSetSharedStorageTracking p = void <<< FFI.promiseToAff <<< send p "Storage.setSharedStorageTracking"

storageSetStorageBucketTracking :: Foreign -> CDPSession -> Aff Unit
storageSetStorageBucketTracking p = void <<< FFI.promiseToAff <<< send p "Storage.setStorageBucketTracking"

storageDeleteStorageBucket :: Foreign -> CDPSession -> Aff Unit
storageDeleteStorageBucket p = void <<< FFI.promiseToAff <<< send p "Storage.deleteStorageBucket"

storageRunBounceTrackingMitigations :: Foreign -> CDPSession -> Aff Foreign
storageRunBounceTrackingMitigations p = FFI.promiseToAff <<< send p "Storage.runBounceTrackingMitigations"

storageSetAttributionReportingLocalTestingMode :: Foreign -> CDPSession -> Aff Unit
storageSetAttributionReportingLocalTestingMode p = void <<< FFI.promiseToAff <<< send p "Storage.setAttributionReportingLocalTestingMode"

storageSetAttributionReportingTracking :: Foreign -> CDPSession -> Aff Unit
storageSetAttributionReportingTracking p = void <<< FFI.promiseToAff <<< send p "Storage.setAttributionReportingTracking"

systemInfoGetInfo :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetInfo p = FFI.promiseToAff <<< send p "SystemInfo.getInfo"

systemInfoGetFeatureState :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetFeatureState p = FFI.promiseToAff <<< send p "SystemInfo.getFeatureState"

systemInfoGetProcessInfo :: Foreign -> CDPSession -> Aff Foreign
systemInfoGetProcessInfo p = FFI.promiseToAff <<< send p "SystemInfo.getProcessInfo"

targetActivateTarget :: Foreign -> CDPSession -> Aff Unit
targetActivateTarget p = void <<< FFI.promiseToAff <<< send p "Target.activateTarget"

targetAttachToTarget :: Foreign -> CDPSession -> Aff Foreign
targetAttachToTarget p = FFI.promiseToAff <<< send p "Target.attachToTarget"

targetAttachToBrowserTarget :: Foreign -> CDPSession -> Aff Foreign
targetAttachToBrowserTarget p = FFI.promiseToAff <<< send p "Target.attachToBrowserTarget"

targetCloseTarget :: Foreign -> CDPSession -> Aff Foreign
targetCloseTarget p = FFI.promiseToAff <<< send p "Target.closeTarget"

targetExposeDevToolsProtocol :: Foreign -> CDPSession -> Aff Unit
targetExposeDevToolsProtocol p = void <<< FFI.promiseToAff <<< send p "Target.exposeDevToolsProtocol"

targetCreateBrowserContext :: Foreign -> CDPSession -> Aff Foreign
targetCreateBrowserContext p = FFI.promiseToAff <<< send p "Target.createBrowserContext"

targetGetBrowserContexts :: Foreign -> CDPSession -> Aff Foreign
targetGetBrowserContexts p = FFI.promiseToAff <<< send p "Target.getBrowserContexts"

targetCreateTarget :: Foreign -> CDPSession -> Aff Foreign
targetCreateTarget p = FFI.promiseToAff <<< send p "Target.createTarget"

targetDetachFromTarget :: Foreign -> CDPSession -> Aff Unit
targetDetachFromTarget p = void <<< FFI.promiseToAff <<< send p "Target.detachFromTarget"

targetDisposeBrowserContext :: Foreign -> CDPSession -> Aff Unit
targetDisposeBrowserContext p = void <<< FFI.promiseToAff <<< send p "Target.disposeBrowserContext"

targetGetTargetInfo :: Foreign -> CDPSession -> Aff Foreign
targetGetTargetInfo p = FFI.promiseToAff <<< send p "Target.getTargetInfo"

targetGetTargets :: Foreign -> CDPSession -> Aff Foreign
targetGetTargets p = FFI.promiseToAff <<< send p "Target.getTargets"

targetSendMessageToTarget :: Foreign -> CDPSession -> Aff Unit
targetSendMessageToTarget p = void <<< FFI.promiseToAff <<< send p "Target.FFI.promiseToAff <<< sendMessageToTarget"

targetSetAutoAttach :: Foreign -> CDPSession -> Aff Unit
targetSetAutoAttach p = void <<< FFI.promiseToAff <<< send p "Target.setAutoAttach"

targetAutoAttachRelated :: Foreign -> CDPSession -> Aff Unit
targetAutoAttachRelated p = void <<< FFI.promiseToAff <<< send p "Target.autoAttachRelated"

targetSetDiscoverTargets :: Foreign -> CDPSession -> Aff Unit
targetSetDiscoverTargets p = void <<< FFI.promiseToAff <<< send p "Target.setDiscoverTargets"

targetSetRemoteLocations :: Foreign -> CDPSession -> Aff Unit
targetSetRemoteLocations p = void <<< FFI.promiseToAff <<< send p "Target.setRemoteLocations"

tetheringBind :: Foreign -> CDPSession -> Aff Unit
tetheringBind p = void <<< FFI.promiseToAff <<< send p "Tethering.bind"

tetheringUnbind :: Foreign -> CDPSession -> Aff Unit
tetheringUnbind p = void <<< FFI.promiseToAff <<< send p "Tethering.unbind"

tracingEnd :: CDPSession -> Aff Unit
tracingEnd = void <<< FFI.promiseToAff <<< send0 "Tracing.end"

tracingGetCategories :: Foreign -> CDPSession -> Aff Foreign
tracingGetCategories p = FFI.promiseToAff <<< send p "Tracing.getCategories"

tracingRecordClockSyncMarker :: Foreign -> CDPSession -> Aff Unit
tracingRecordClockSyncMarker p = void <<< FFI.promiseToAff <<< send p "Tracing.recordClockSyncMarker"

tracingRequestMemoryDump :: Foreign -> CDPSession -> Aff Foreign
tracingRequestMemoryDump p = FFI.promiseToAff <<< send p "Tracing.requestMemoryDump"

tracingStart :: Foreign -> CDPSession -> Aff Unit
tracingStart p = void <<< FFI.promiseToAff <<< send p "Tracing.start"

fetchDisable :: CDPSession -> Aff Unit
fetchDisable = void <<< FFI.promiseToAff <<< send0 "Fetch.disable"

fetchEnable :: Foreign -> CDPSession -> Aff Unit
fetchEnable p = void <<< FFI.promiseToAff <<< send p "Fetch.enable"

fetchFailRequest :: Foreign -> CDPSession -> Aff Unit
fetchFailRequest p = void <<< FFI.promiseToAff <<< send p "Fetch.failRequest"

fetchFulfillRequest :: Foreign -> CDPSession -> Aff Unit
fetchFulfillRequest p = void <<< FFI.promiseToAff <<< send p "Fetch.fulfillRequest"

fetchContinueRequest :: Foreign -> CDPSession -> Aff Unit
fetchContinueRequest p = void <<< FFI.promiseToAff <<< send p "Fetch.continueRequest"

fetchContinueWithAuth :: Foreign -> CDPSession -> Aff Unit
fetchContinueWithAuth p = void <<< FFI.promiseToAff <<< send p "Fetch.continueWithAuth"

fetchContinueResponse :: Foreign -> CDPSession -> Aff Unit
fetchContinueResponse p = void <<< FFI.promiseToAff <<< send p "Fetch.continueResponse"

fetchGetResponseBody :: Foreign -> CDPSession -> Aff Foreign
fetchGetResponseBody p = FFI.promiseToAff <<< send p "Fetch.getResponseBody"

fetchTakeResponseBodyAsStream :: Foreign -> CDPSession -> Aff Foreign
fetchTakeResponseBodyAsStream p = FFI.promiseToAff <<< send p "Fetch.takeResponseBodyAsStream"

webAudioEnable :: CDPSession -> Aff Unit
webAudioEnable = void <<< FFI.promiseToAff <<< send0 "WebAudio.enable"

webAudioDisable :: CDPSession -> Aff Unit
webAudioDisable = void <<< FFI.promiseToAff <<< send0 "WebAudio.disable"

webAudioGetRealtimeData :: Foreign -> CDPSession -> Aff Foreign
webAudioGetRealtimeData p = FFI.promiseToAff <<< send p "WebAudio.getRealtimeData"

webAuthnEnable :: Foreign -> CDPSession -> Aff Unit
webAuthnEnable p = void <<< FFI.promiseToAff <<< send p "WebAuthn.enable"

webAuthnDisable :: CDPSession -> Aff Unit
webAuthnDisable = void <<< FFI.promiseToAff <<< send0 "WebAuthn.disable"

webAuthnAddVirtualAuthenticator :: Foreign -> CDPSession -> Aff Foreign
webAuthnAddVirtualAuthenticator p = FFI.promiseToAff <<< send p "WebAuthn.addVirtualAuthenticator"

webAuthnSetResponseOverrideBits :: Foreign -> CDPSession -> Aff Unit
webAuthnSetResponseOverrideBits p = void <<< FFI.promiseToAff <<< send p "WebAuthn.setResponseOverrideBits"

webAuthnRemoveVirtualAuthenticator :: Foreign -> CDPSession -> Aff Unit
webAuthnRemoveVirtualAuthenticator p = void <<< FFI.promiseToAff <<< send p "WebAuthn.removeVirtualAuthenticator"

webAuthnAddCredential :: Foreign -> CDPSession -> Aff Unit
webAuthnAddCredential p = void <<< FFI.promiseToAff <<< send p "WebAuthn.addCredential"

webAuthnGetCredential :: Foreign -> CDPSession -> Aff Foreign
webAuthnGetCredential p = FFI.promiseToAff <<< send p "WebAuthn.getCredential"

webAuthnGetCredentials :: Foreign -> CDPSession -> Aff Foreign
webAuthnGetCredentials p = FFI.promiseToAff <<< send p "WebAuthn.getCredentials"

webAuthnRemoveCredential :: Foreign -> CDPSession -> Aff Unit
webAuthnRemoveCredential p = void <<< FFI.promiseToAff <<< send p "WebAuthn.removeCredential"

webAuthnClearCredentials :: Foreign -> CDPSession -> Aff Unit
webAuthnClearCredentials p = void <<< FFI.promiseToAff <<< send p "WebAuthn.clearCredentials"

webAuthnSetUserVerified :: Foreign -> CDPSession -> Aff Unit
webAuthnSetUserVerified p = void <<< FFI.promiseToAff <<< send p "WebAuthn.setUserVerified"

webAuthnSetAutomaticPresenceSimulation :: Foreign -> CDPSession -> Aff Unit
webAuthnSetAutomaticPresenceSimulation p = void <<< FFI.promiseToAff <<< send p "WebAuthn.setAutomaticPresenceSimulation"

mediaEnable :: CDPSession -> Aff Unit
mediaEnable = void <<< FFI.promiseToAff <<< send0 "Media.enable"

mediaDisable :: CDPSession -> Aff Unit
mediaDisable = void <<< FFI.promiseToAff <<< send0 "Media.disable"

deviceAccessEnable :: CDPSession -> Aff Unit
deviceAccessEnable = void <<< FFI.promiseToAff <<< send0 "DeviceAccess.enable"

deviceAccessDisable :: CDPSession -> Aff Unit
deviceAccessDisable = void <<< FFI.promiseToAff <<< send0 "DeviceAccess.disable"

deviceAccessSelectPrompt :: Foreign -> CDPSession -> Aff Unit
deviceAccessSelectPrompt p = void <<< FFI.promiseToAff <<< send p "DeviceAccess.selectPrompt"

deviceAccessCancelPrompt :: Foreign -> CDPSession -> Aff Unit
deviceAccessCancelPrompt p = void <<< FFI.promiseToAff <<< send p "DeviceAccess.cancelPrompt"

preloadEnable :: CDPSession -> Aff Unit
preloadEnable = void <<< FFI.promiseToAff <<< send0 "Preload.enable"

preloadDisable :: CDPSession -> Aff Unit
preloadDisable = void <<< FFI.promiseToAff <<< send0 "Preload.disable"

fedCmEnable :: Foreign -> CDPSession -> Aff Unit
fedCmEnable p = void <<< FFI.promiseToAff <<< send p "FedCm.enable"

fedCmDisable :: CDPSession -> Aff Unit
fedCmDisable = void <<< FFI.promiseToAff <<< send0 "FedCm.disable"

fedCmSelectAccount :: Foreign -> CDPSession -> Aff Unit
fedCmSelectAccount p = void <<< FFI.promiseToAff <<< send p "FedCm.selectAccount"

fedCmDismissDialog :: Foreign -> CDPSession -> Aff Unit
fedCmDismissDialog p = void <<< FFI.promiseToAff <<< send p "FedCm.dismissDialog"

fedCmResetCooldown :: CDPSession -> Aff Unit
fedCmResetCooldown = void <<< FFI.promiseToAff <<< send0 "FedCm.resetCooldown"
