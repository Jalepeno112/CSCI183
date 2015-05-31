import pandas as pd 
import destinyPlatform as destiny 
import json
import os
import numpy as np


def getWeaponRatiosByMap(data):
	weaponColumns = [c for c in data.columns if 'weapon' in c and 'Heavy' not in c and 'Secondary' not in c and 'Primary' not in c]
	groupedByMap = data.groupby('refrencedId')

	mapDict = {h:destiny.getMapName(h) for h in groupedByMap.groups.keys()}

	weaponRatios = {}
	weaponRatios = [{'key':c, 'values': [{'x':mapDict[map], 
							'y': groupedByMap.get_group(map)[c].sum()/groupedByMap.get_group(map)['kills'].sum()} for map in groupedByMap.groups.keys()]
					}for c in weaponColumns]

	return weaponRatios

def quittingByMap(data):
	teamKeys = [16, 17]
	teamKeysToTitles = {16:'Alpha', 17:"Bravo"}

	data = data[(data['team'] == 16) | (data['team'] == 17)]
	groupedByMapTeam = data.groupby(['refrencedId', 'team'])

	mapDict = {h:destiny.getMapName(h) for h,_ in groupedByMapTeam.groups.keys()}

	quittingByMap = [{
						'key': teamKeysToTitles[team],
						'values': [{
									'x':mapDict[map],
									'y': 1.0 - (groupedByMapTeam.get_group((map, team))['completed'].sum()/float(len(groupedByMapTeam.get_group((map,team))))),
									} for map,_ in groupedByMapTeam.groups.keys() if (map,team) in groupedByMapTeam.groups.keys()]
					} for team in teamKeys]
	with open(os.path.join('..','gh-pages', 'datafiles', 'quittingByMap.json'), 'w') as f:
		json.dump(quittingByMap, f)

def dominationByMap(data):
	groupedByMapStanding = data.groupby(['refrencedId', 'standing'])

	victoryToString ={0:"Winners",1:"Losers"}
	mapDict = {h:destiny.getMapName(h) for h,_ in groupedByMapStanding.groups.keys()}

	dominationKills = [{'key':victoryToString[v], 
						'values': [{
							'x': mapDict[map],
							'y': float(groupedByMapStanding.get_group((map, v))['dominationKills'].mean())
						} for map, _ in groupedByMapStanding.groups.keys()]
					}for v in victoryToString.keys()]
	with open(os.path.join('..','gh-pages', 'datafiles', 'dominationKills.json'), 'w') as f:
		json.dump(dominationKills, f)

def victoryByMapAndTeam(data):
	teamKeys = [16, 17]

	teamKeysToTitles = {16:'Alpha', 17:"Bravo"}

	groupedByMapTeam = data.groupby(['refrencedId', 'team'])

	mapDict = {h:destiny.getMapName(h) for h,_ in groupedByMapTeam.groups.keys()}

	victoryByMap = [{
						'key': teamKeysToTitles[team],
						'values': [{
									'x':mapDict[map],
									'y': 1.0 - (groupedByMapTeam.get_group((map, team))['standing'].sum()/float(len(groupedByMapTeam.get_group((map,team))))),
									} for map,_ in groupedByMapTeam.groups.keys() if (map,team) in groupedByMapTeam.groups.keys()]
					} for team in teamKeys]
	with open(os.path.join('..','gh-pages', 'datafiles', 'victoryByMap.json'), 'w') as f:
		json.dump(victoryByMap, f)


def weaponsByClass(data):
	data = data[data['characterClass'] != '0']
	groupByClass = data.groupby(['characterClass'])
	weaponColumns = [c for c in data.columns if 'weapon' in c and 'Heavy' not in c and 'Secondary' not in c and 'Primary' not in c]

	weaponRatios = [{'key':w, 
					'values': [{
								'x': c, 
								'y': float(groupByClass.get_group(c)[w].sum())/groupByClass.get_group(c)['kills'].sum()
							} for c in groupByClass.groups.keys()]
					}for w in weaponColumns]
	with open(os.path.join('..','gh-pages', 'datafiles', 'weaponsByClass.json'), 'w') as f:
		json.dump(weaponRatios, f)

def objectivesByMap(data):
	groupedByMapStanding = data.groupby(['refrencedId', 'standing'])

	victoryToString ={0:"Winners",1:"Losers"}
	mapDict = {h:destiny.getMapName(h) for h,_ in groupedByMapStanding.groups.keys()}

	objectivesCompleted = [{'key':victoryToString[v], 
						'values': [{
							'x': mapDict[map],
							'y': float(groupedByMapStanding.get_group((map, v))['objectivesCompleted'].mean())
						} for map, _ in groupedByMapStanding.groups.keys()]
					}for v in victoryToString.keys()]
	with open(os.path.join('..','gh-pages', 'datafiles', 'objectivesCompleted.json'), 'w') as f:
		json.dump(objectivesCompleted, f)

def scorePerKill(data):
	groupedByMapStanding = data.groupby(['refrencedId', 'standing'])

	victoryToString ={0:"Winners",1:"Losers"}
	mapDict = {h:destiny.getMapName(h) for h,_ in groupedByMapStanding.groups.keys()}

	averageScorePerKill = [{'key':victoryToString[v], 
						'values': [{
							'x': mapDict[map],
							'y': float(groupedByMapStanding.get_group((map, v))['averageScorePerKill'].mean())
						} for map, _ in groupedByMapStanding.groups.keys()]
					}for v in victoryToString.keys()]
	with open(os.path.join('..','gh-pages', 'datafiles', 'averageScorePerKills.json'), 'w') as f:
		json.dump(averageScorePerKill, f)

def getSniperRatiosByVictory(data):
	victoryToString ={0:"Winners",1:"Losers"}


	groupedByMapStanding = teamData.groupby(['refrencedId', 'standing'])

	mapDict = {h[0]:destiny.getMapName(h[0]) for h in groupedByMapStanding.groups.keys()}


	sniperRatios = [{'key':victoryToString[v], 
						'values': [{
							'x': mapDict[map],
							'y': groupedByMapStanding.get_group((map, v))['weaponKillsSniper'].sum() / groupedByMapStanding.get_group((map,v))['kills'].sum()
						} for map, _ in groupedByMapStanding.groups.keys()]
					}for v in victoryToString.keys()]
	return sniperRatios

def classLevelVictory(data):
	"""
	Build json file for a graph that displays how frequent a class wins based on their level
	"""

	data = pd.read_csv("data.csv")

	data = data[data['characterClass'] != '0']
	data = data[data['characterLevel'] != 0]

	victoryToString = {0:"Winners", 1:"Losers"}

	groupByClass = data.groupby(['characterClass', 'characterLevel'])

	print("Formatting data")

	characterKeys = ['Warlock', 'Titan', 'Hunter']
	levelKeys = list(set([level for _,level in groupByClass.groups.keys()]))

	victoryByClassLevel = [{'key':character, 
							'values': [{
								'x': level,
								'y': 1.0 - (float((groupByClass.get_group((character, level))['standing'].sum())/len(groupByClass.get_group((character,level)))))
							} for  level in levelKeys if (character,level) in groupByClass.groups.keys()]
						}for character in characterKeys]


	with open(os.path.join('..','gh-pages', 'datafiles', 'characterClassVictory.json'), 'w') as f:
		json.dump(victoryByClassLevel, f)
	return victoryByClassLevel

def predictedVsActual(actual, predicted):
	"""
	Build the json object for a graph that displays each team's actual victory and defeat by game, and then the predicted probability of their victory
	"""

	victoryToString ={0:"Winners", 1:"Losers"}
	teamToString = {16: 'Alpha', 17:'Bravo'}

	actual_groupedByTeam = actual.groupby(['team', 'gameId'])

	teamKeys = [16, 17]
	gameIdKeys = list(set([gameId for _,gameId in actual_groupedByTeam.groups.keys()]))

	print("Formatting actual data")

	actual_json = [{'key':teamToString[team], 
							'values': [{
								'x': float(gameId),
								'y': int(actual_groupedByTeam.get_group((team,gameId))['standing'])
							} for gameId in gameIdKeys if (team,gameId) in actual_groupedByTeam.groups.keys()]
						 } for team in teamKeys]

	print("Formatting predicted data")
	predicted_groupedByTeam = predicted.groupby(['team','gameId'])
	gameIdKeys = list(set([gameId for _,gameId in actual_groupedByTeam.groups.keys()]))

	predicted_json = [{'key':teamToString[team] +' Probability Of Victory',
							'values': [{
								'x': float(gameId),
								'y': float(predicted_groupedByTeam.get_group((team,gameId))['probabilityOfVictory'])
							} for gameId in gameIdKeys if (team,gameId) in predicted_groupedByTeam.groups.keys()]
						 } for team in teamKeys]

	return (actual_json + predicted_json)



if __name__ == "__main__":
	teamData = pd.read_csv("teamData.csv")

	"""
	weaponRatios = getWeaponRatiosByMap(teamData)

	with open(os.path.join('..','gh-pages','datafiles',"weapon_sums.json"), 'w') as f:
		json.dump(weaponRatios, f)
	

	sniperRatios = getSniperRatiosByVictory(teamData)
	with open(os.path.join('..','gh-pages', 'datafiles', 'sniperRatioVictory.json'), 'w') as f:
		json.dump(sniperRatios, f)

"""
	actual = teamData.ix[(len(teamData)/2 + 1):len(teamData), ]
	predicted = pd.read_csv("predictions_binomial.csv")

	actualVsPredicted = predictedVsActual(actual, predicted)
	print("writing to file")

	with open(os.path.join('..','gh-pages', 'datafiles', 'actualVsPredicted.json'), 'w') as f:
		json.dump(actualVsPredicted, f)

	#classLevelVictory(None)