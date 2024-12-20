Proposal-Spotify 30000
================
Zhengkun Ou, Yulin Liu, Wanlin Wu, Congyu Yang, Senna Kim
2024-11-08

# Group Information

- **Group Members (names and UNIs)**
  - Yulin Liu, UNI: yl5829
  - Wanlin Wu, UNI: ww2745
  - Congyu Yang, UNI: cy2751
  - Senna Kim, UNI: sk5538
  - Zhengkun Ou, UNI: zo2168

# Project Title

**What makes your song shiny on Spotify?**

# Motivation for the Project

This project may provide insights useful for creators to understand: -
Current music trends - Factors that contribute to a song’s popularity -
Music trends over time - Genre analysis and diversity

# Intended Final Product

The optimal combination of music features for each genre/time period.

# Anticipated Data Sources

- [Spotify 30000 Songs Dataset on
  Kaggle](https://www.kaggle.com/datasets/joebeachcapital/30000-spotify-songs)

# Planned Analyses / Visualizations / Coding Challenges

1.  **Correlation Matrix of Music Features**
    - Heat map visualization
2.  **Danceability vs. Popularity**
    - Is a more danceable song more likely to be popular?
    - Analysis:
      - Correlation using scatter plot
      - Grouped by genre
3.  **Comfort Range for Keys**
    - Is there a range of keys that people are most comfortable with?
    - Analysis:
      - Correlation using scatter plot
      - Grouped by genre
4.  **Will long duration songs be less likely to be more popular?**
    - First categorize songs into long-duration/short duration
    - Boxplot: scores and duration
    - Group_by genre
5.  **Which factors would contribute more to the popularity?**
    - distribution: scatterplot with colors
6.  **Trends of popularity of different genres based on release date?**
    - Which genre was released the most over time, and how does it
      correlate with popularity? Did Covid affect this?
    - How the release of popular of genres changes over time/within a
      year

## The Spotify Popularity Index

The Spotify Popularity Index is an internal metric used by Spotify to
measure the relative popularity of a song or artist on its platform. It
ranges from 0 to 100, with higher numbers indicating greater popularity.
It’s typically only available to developers via the Spotify API.

### Calculation Factors

The index is calculated using various factors, including:

1.  Number of Plays: How many times the track has been played
2.  Recency: More recent plays have a higher weight than older ones
3.  Share of Total Plays: Popularity is relative to other songs in the
    catalogue
4.  User Engagement: Including saves, shares, and playlist additions
5.  Skip Rate: Fewer skips means increased popularity

### Current Limitation

Limitation: we only have the album_release_date, we are not able to
analyze the trend among years.

## Project Timeline

**Final due date: 12/7**

### Schedule Breakdown

- **11/11-15**: Project review meeting with TA
- **11/18-22**: Analysis + visualization
- **11/25-29**: Create report and web page
- **12/2-6**: Create webcast and edit web page
- **12/7**: Submit
