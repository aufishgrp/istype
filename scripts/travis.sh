function set_version(){
  VERSION=`cat VERSION`;
  if [ "`git rev-list ${VERSION}.0`" ]; then
    VERSION=${VERSION}.`git rev-list --merges --count ${VERSION}.0..`
  else
    VERSION=${VERSION}.0
  fi
}

function set_last_version(){
  LAST_VERSION=`git describe --tags --abbrev=0`
}

function set_new_version(){
  set_version
  if [ "`git rev-list ${VERSION} 2>/dev/null`" ]; then
    NEW_VERSION="false"
  else
    NEW_VERSION="true"
  fi
}

function create_release_notes(){
  set_last_version
  RELEASE_NOTES=`git log ${LAST_VERSION}.. --pretty=format:"<li><a href='http://github.com/${GIT_OWNER}/${GIT_REPO}/commit/%H'>view commit &bull;</a>%s</li>" --reverse --no-merges -i --invert-grep --grep="Travis"`   
}

function create_json(){
  set_version
  create_release_notes
  JSON="{\"tag_name\":\"${VERSION}\", \"name\":\"${VERSION}\", \"target_commitish\":\"master\", \"draft\":false, \"prerelease\":false, \"body\":\"${RELEASE_NOTES}\"}"
}

function create_tag(){
  set_version
  echo "Creating release ${VERSION}"

  create_json
  echo ${JSON} | http POST https://api.github.com/repos/${GIT_OWNER}/${GIT_REPO}/releases Authorization:"token ${GIT_KEY}"
}

function publish_hex(){
  set_version
  echo "Publishing release ${VERSION}"

  ## Configure hex
  mkdir -p ~/.hex
  echo '{username,<<"'${HEX_USERNAME}'">>}.' > ~/.hex/hex.config
  echo '{key,<<"'${HEX_KEY}'">>}.' >> ~/.hex/hex.config
  
  ## Configure rebar3
  mkdir -p ~/.config/rebar3
  echo '{plugins, [rebar3_hex]}.' > ~/.config/rebar3/rebar.config
  printf "y\n" | rebar3 hex publish
}

set_version
set_new_version
set_last_version

echo "Version history ${LAST_VERSION} | ${VERSION} | ${NEW_VERSION}"

if [ "${TRAVIS_PULL_REQUEST}" == "false" ]; then
  if [ "${TRAVIS_BRANCH}" == "master" ]; then
    if [ "${NEW_VERSION}" == "true" ]; then
      create_tag
    else
      echo "Version unchanged: No publication actions taken."
    fi
  elif [ "${TRAVIS_TAG}" ]; then
    publish_hex    
  fi
else
  echo "Pull request: No publication actions taken."
fi
