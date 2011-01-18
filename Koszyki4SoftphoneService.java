/* (c) 2009 POLKOMTEL S.A.
 * All rights reserved.
 *
 * The contents of this file are CONFIDENTIAL and 
 * PROPRIETARY information of POLKOMTEL S.A.
 * Use is subject to license terms.
 */
package pl.com.javart.koszyki.postpaid.ucm;

import pl.com.javart.koszyki.postpaid.ucm.model.AgentStateMessage;
import pl.com.javart.koszyki.postpaid.ucm.model.CallContext;
import pl.com.javart.koszyki.postpaid.ucm.model.Config;
import pl.com.javart.koszyki.postpaid.ucm.model.EventInfo;
import pl.com.javart.koszyki.postpaid.ucm.model.PhoneStateMessage;

public interface Koszyki4SoftphoneService {

	/**
	 * Called when softphone is fired. When loading, it's trying to get configuration from Koszyki
	 * which contains particular parameters for current agent (i.e. delay before/after call). 
	 *  
	 * @param login
	 * @param domain domain name (i.e. 'POLKOMTEL')
	 * @return config object containing current agent parameters which are sotred in koszyki app.
	 */
	Config getConfiguration(String login, String domain);

	/**
	 * Called by Softphone, when agent changes it's state.
	 * This method is useful for storing statistics of agent states.
	 *  
	 * @param stateMessage
	 * @see {@link AgentStateMessage}
	 */
	void agentStateChanged(AgentStateMessage stateMessage);

	/**
	 * Called by Softphone, when phone changes it's state.
	 * This method is used to publish events info on case details page.
	 *  
	 * @param stateMessage
	 * @see {@link PhoneStateMessage}
	 */
	void phoneStateChanged(PhoneStateMessage stateMessage);

	/**
	 * Called periodically (only after agent logs into Softphone),
	 * and returns current event to dial.
	 * 
	 * @param agentId agent login, because in general, Koszyki disallow to dial event which is assigned to different agent
	 * @return object containing several parameters about current event to dial
	 * @see {@link EventInfo}
	 */
	EventInfo getEventToDial(String agentId);

	/**
	 * Called when current call is finished. After call is finished, several actions are made, such as
	 * changing case status etc.
	 * 
	 * @param callContext object containing 
	 */
	void processFinishedCall(CallContext callContext);
}
